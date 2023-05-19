---@TODO remove exp_ and stat_ prefixes
---@TODO vague floating idea : make code from AST. Put comments/annotations in AST
---enables pretty print, automatic documentation, helps introspection, debugging, reflexivity.
---helps changing syntax.
local pt = require "pt".pt
local lpeg = require "lpeg"

local Stack = require"Stack"
local Node = require"ASTNode"

local nodeNum = Node.nodeNum
local nodeBinop = Node.nodeBinop
local nodeFoldBinop = Node.nodeFoldBinop
local nodeFoldBinopSuffix = Node.nodeFoldBinopSuffix
local nodeUnaryop = Node.nodeUnaryop
local nodeAssign = Node.nodeAssign

require "utils"

--TODO : use _ENV to put in a module. to call, for example, _ENV = require "require"
local P = lpeg.P
local S = lpeg.S
local R = lpeg.R
local B = lpeg.B
local V = lpeg.V

local C = lpeg.C
local Carg = lpeg.Carg
local Cargs = lpeg.Cargs
local Cb = lpeg.Cb
local Cc = lpeg.Cc
local Cf = lpeg.Cf
local Cp = lpeg.Cp
local Cs = lpeg.Cs
local Ct = lpeg.Ct
local Cmt = lpeg.Cmt

local I = lpeg.I
---lpeg Fold capture right associative
local Cfr = lpeg.Cfr

--------------------------------------------------------------------------------
--elementary patterns

---@TODO make msg an object ??
---@TODO use a patt argument for nested/chained errors ??
local function err(msg)
    return --Cmt('', 
        function (subject, p, ...)
            local lineStart = 1
            local _, lineNumber = subject:sub(1, p):gsub("()\n", function (pos) lineStart = pos end)
            io.stderr:write(string.format("error in <input>:%d:%d\t", lineNumber + 1, p - lineStart + 1))    --TODO use filename
            io.stderr:write(msg .. '\n')
            os.exit()
            return false, msg
        end
    --)
end


local locale = lpeg.locale()

local blockComment = "'{" * (P(1) - "'}")^0 * "'}"
local lineComment = "'" * (P(1) - "\n")^0
local comment = blockComment + lineComment
local newLine = '\n' --  * Cg(Cb("lineCount") / inc, "lineCount") * Cg(Cp(), "lastLineStart")
---@TODO put spaces inside the grammar
--local ws = V'ws'
--local ws = V'ws_'
local ws = newLine + locale.space + comment    --we might need ws or ws^1 in some places
local ws_ = ws^0

---@TODO think about what a token is and isn''t, and about spacing (operators probably aren''t token)
--token generator
local function T_(token)
    return token * ws_
end
----set token generator
--local function Ts_(tokens)
--    return S(tokens) * ws_
--end

---@TODO ponder whether they should be, empty statements, list of statements, etc. Probably still no.
local semicolon_ = T_";"^-1 * (- T_";" + err"useless semi-colons are not allowed, you peasant!")
local semicolons_ = T_";"^0


local function numeralCapture(digit, comma)
    return (digit^1 + digit^0 * comma * digit^1) -- ((comma+"")*-(digit+comma))
end
local comma = S'.'

--local digit = R'09'
local digit = locale.digit
local digits = digit^0
--local xdigit = R('09', 'af', 'AF')
local xdigit = locale.xdigit
local xdigits = xdigit^0
---@TODO if no other pattern starts with a digit, or depending on the ordering of the choice, loosen this pattern and raise a more informative error
--I might want to have coding numeral stuck to variable identifiers or very special operators, so no spaces at the end.
local numeral = Cmt(('0' * S'xX' * numeralCapture(xdigit, comma) + numeralCapture(digit, comma) * (S'eE' * digit^1)^-1),
    function (subject, position, n)
        n = tonumber(n)
        if n then
            return true, n
        else
            return err"malformed numeral"(subject, position)
        end
    end)
    / nodeNum

local alpha = locale.alpha
--local alpha = R('az', 'AZ')
--local alnum = alpha+digit
local alnum = locale.alnum

--spaces
---@TODO : store lineCount and lastLineStart inside or around the AST for reporting and syntax highlight
---@TODO : use doo/doone/doon't for block comments ? Answer : no, it's moronic and conflicts violently with getting rid of keywords, unless you go full functional curry, which I won't.

---@TODO alleviate the need for reserved words (still have special words)
local Rw_ = setmetatable({
    "return",
    "if",
    "else",
    "therefore",
    "goto",
    "while",
    "new",
    "Array",    ---@TODO (after function and OO) : alternative way to declare Arrays, to eventually replace `new` keyword
},{__call = function(self, word)
    return self[word]
end,
    __index = function(self, key)
        error("Not a reserved word: " .. key, 3)
    end,
})

for i = 1, #Rw_ do
    --for _, w in ipairs(Rw_) do    --ipairs apparently changed from 5.2 to 5.4, the stop case is no longer i > #t, but t[i] == nil instead, triggering __index.
    --I thought ipairs was supposed to be depreciated at some point, but I'm happy to see it sticking
    --this change is probably for the best, it now allows to do some stupid things like virtual/dynamic/infinite arrays using ipairs.
    local w = Rw_[i]
    Rw_[w] = w * - alnum * ws_
--    Rw = Rw + w
end
--Rw = Rw * - alnum

--no spaces, potentially, things like field access need to be stuck to the ID
--the possibility of no spaces before seems more important though
local ID = Cmt((alpha + '_') * (alnum + '_')^0, function (_, p, w)
    return rawget(Rw_, w) == nil, w
end)
local var = ID / Node{tag = 'variable', "var"}

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
---expressions and statements

local _parenNames = {
    ["("] = "parentheses",
    ["["] = "bracket",
    ["{"] = "brace",
}
---@TODO also checks for extra closing ones ? (maybe not here).
local function paren_ (op, patt, cl, pattName, bname)
    bname = bname or _parenNames[op]
    return T_(op) * patt * (T_(cl) + err(pattName .. ": missing closing " .. bname .. "."))
end

local indexed_ = Cf(var * V'ws_' * paren_("[", V'exp_',"]", "Array index")^0, Node{tag = 'indexed', 'ref', 'index'})

--- print. Because it is prefix, I'm notindexed gonna put it in formula, so (@ = a) rather than @ =(a) is necessary to disambiguate.<br>
--- Potentially allows for depth 1 shenanigans, where we print more than just the value.
local ref_ = T_"@" / Node{tag = 'io'}
    + indexed_
    --Cf(var * V'ws_' * paren_("[", V'exp_',"]", "Array index")^0, Node{tag = 'indexed', 'ref', 'index'})

--[[
    function pattern
```
a # = exp
```
Means `a` is assigned to a function that yields `exp`.
The logic is the same as pointers in C/C++ : a is assigned to the value such that `a#` evaluates to `exp`.

TODO add anonymous statements : `(#= exp)`

TODO : add  chaining `#` syntax. As lhs :
`a## = exp` is equivalent to `a # = (#= exp)`<br>
As rhs : `a##` is equivalent to `(a#)#` (should be free ?)
]]
local fun_ = Cf(V'indexed_' * T_(C"#")^1, Node{tag = "fun", "ref"})  --function calls

---@TODO : make every statement expression.
-- a list of constructs useable to build expression, from highest to lowest priority.
-- in my understanding, priority order is really only needed for pattern needing disambiguation, such as infix op pattern.
local exp_ = Stack{'seqs_', --'exp_',
    ws_ = ws_,
    indexed_ = indexed_,
    ref_ = ref_,
    --an expressions or expression sequence delimited by parntheses
    group_ = paren_("(", V'seqs_',")", "group"),
    fun_ = fun_,
}

--------------------------------------------------------------------------------
---formula

--elaborate patterns utils for formula

local function infixOpCapture(opPatt, abovePattern)
    return Cf(abovePattern * (opPatt * abovePattern / nodeFoldBinopSuffix)^0, nodeFoldBinop)
end
local function infixOpCaptureRightAssoc(opPatt, selfPattern, abovePattern, tag)  --set self to above to have a non-asociative binary op.
    --return Cg(abovePattern, '_') * Cg(Cb('_') * (opPatt * selfPattern / nodeFoldBinopSuffix) / nodeFoldBinop, '_')^-1 * Cb('_') --overkill.
    return abovePattern * (opPatt * selfPattern / nodeFoldBinopSuffix)^-1 / Node(Node.isEmpty, 2, {1}, {2, {tag = tag or 'binop', 'exp1'}})
end
local function unaryOpCapture(opPatt, selfPattern, abovePattern)  --allows chaining. set self to above to disallow
    return abovePattern + opPatt * ws_ * abovePattern / nodeUnaryop + opPatt * ws^1 * selfPattern / nodeUnaryop --not allowing ++ , -- or +- , but allowing - -
end
local function infixChainCapture(opPatt, abovePattern, tag)
    return abovePattern * (opPatt * abovePattern)^0 / Node(Node.isEmpty, 3, {1}, {{tag = tag or 'chainop', [0] = 'chain'}})
end

--primary, aka frst stage in formulas
exp_:push(
    numeral * V'ws_'
    + V'fun_'
    + ref_
    + V'group_'
    + V'if_'    --dubious here ?
    + V'while_' --dubious here ?
    --+ V'array_' + V'littArray_' --uncomment if you want to put arrays in formula, with some sort of operator overload presumably.
)
exp_:push(infixOpCaptureRightAssoc(C"^" * V'ws_', V(#exp_+1),  V(#exp_))) --power
exp_:push(unaryOpCapture(C(S"+-"), V(#exp_ + 1), V(#exp_))) --unary +-
exp_:push(infixOpCapture(C(S"*/%") * V'ws_', V(#exp_))) --multiplication
exp_:push(infixOpCapture(C(S"+-") * V'ws_', V(#exp_))) --addition
---comparisons create booleans, so having logical operators of lower precedence alow to combine them wihout parentheses makes sense.
exp_:push(infixChainCapture(C(S"<>" * P"="^-1 + S"!=" * "=") * V'ws_', V(#exp_), 'compChain')) --comparison
exp_:push(unaryOpCapture(C"!", V(#exp_ + 1), V(#exp_)))    --unary not.
exp_:push(infixOpCaptureRightAssoc(C"&&" * V'ws_', V(#exp_ + 1), V(#exp_), 'conjunction'))    --binary and.
exp_:push(infixOpCaptureRightAssoc(C"||" * V'ws_', V(#exp_ + 1), V(#exp_), 'disjunction'))    --binary or.
exp_:push(infixOpCaptureRightAssoc(C"=>" * V'ws_', V(#exp_ + 1), V(#exp_), 'imply'))          --logical imply.
--[[
    Formulas are compounds of sticky subexpression, sticky meaning that they may stick to an other higher priority expresion.
    Operator priority is mostly only a thing with infix operators.
    Formula is the top level expression above expression formed above such subexpression.
    ]]
exp_.formula_ = V(#exp_)

--------------------------------------------------------------------------------
---expressions

--array creation
---@TODO add [2,3,4] for flat multidimensional arrays ?
---@TODO remove useless keyword or switch to manual memory management
--exp_._indexChain_ = infixOpCaptureRightAssoc(Cc(nil), V'_indexChain_', brackExp_ + T_"=" * V'exp_', 'new')
--- `brackExp_ + T_"=" * V'exp_'`  : it's ok, T_"=" * V'exp_' is necessarilly last.
---I put it here, it makes more sense than in assign cause it's only useable for initialization, later on writing ̀`myTab = 0` won't fill myTab, it will just set myTab to 0.
exp_.array_ = Cfr(Rw_"new" * paren_("[", V'seqs_', "]", "new Array")^1 * (T_"=" * V'exp_' + Cc(nil)),  --at least on result needed for size specifiers (seqs1_)
    Node{tag='new', 'size', 'default'})
---litteral Array
exp_.littArray_ = paren_("{", V'seqs_', "}", "litteral Array")
    / Node{tag = 'new', 'content'} -- [0] = 'values'}
-----code block. For now, you have to 
--exp_.block_ = paren_("{", V'rawList_', "}", "litteral Array")
--    / Node{tag = 'new', 'content'} -- [0] = 'values'}

local _errNoSC = err"BEWARE, you cannot separate condition, then and else with semi-colon"
--BEWARE, you can't separate condition, then and else with semicolon (that's intentional). You can use them clarify where the whole if stops though.
exp_.if_ = Rw_"if" * V'assign_' * (T_";" * _errNoSC + 0) * V'assign_' *
    ((T_";" * Rw_"else" * _errNoSC + Rw_"else") * V'assign_' *I'else')^-1 /
    Node{tag = 'if', "cond", "then", "else"}
exp_.while_ = Rw_"while" * V'assign_' * V'assign_' / Node{tag = 'while', 'cond', 'stat'}

---@TODO remove
exp_.return_ = Rw_"return" * (V'exp_' + Cc(nil)) / Node{tag = 'return', "exp"}
--exp_.return_ = Rw_"return" * Cc(Node.empty)
--exp_.break_ = Rw_"break" * (V'exp_' + Cc(nil)) / Node{tag = 'break', "exp"}
---@TODO remove return
--- using named pattern variable is not a necessity, here, it could have been a big sum, but it's better for organization, and it's more modular if I want to add only some patterns as LHS for example.
---A pure atomic expression in the sense of expressing a single value (compares with sequences and lists)
exp_. exp_ = (false
    + V'formula_' --contains parentheses group ok.
    + V'array_' + V'littArray_'
    -- if and while here are redundant with exp_[2] (first stage of formula), but they're dubious there and don't hurt here...
    + V'if_'
    + V'while_'
--    + V'break_'
    + V'return_'
)

--------------------------------------------------------------------------------
---sequences


---a function to create sequences patterns.
--- Cc(nil) to escape the pesky full match being returned when no capture occurs (nil is part of the capture but forgotten upon filling the table)
---@TODO error handling ? especially if sep non nil ?
local function seq(patt, sep, nonEmpty, tag, field)
    if sep then
        local r = patt * (sep * patt)^0 * sep^-1
        if not nonEmpty then
            r = r + Cc(nil)
        end
        return r / Node(Node.isEmpty, 2, {Node.isEmpty, 1, {Node.empty}, {1}}, {{tag = tag or 'seq', [0] = field or 'stats'}})
    else
    ---@TODO try ^0.
    ---@TODO Remove Cc(nil) if useless.
    return patt^(nonEmpty and 1 or 0) / Node(Node.isEmpty, 2, {Node.isEmpty, 1, {Node.empty}, {1}}, {{tag = tag or 'seq', [0] = field or 'stats'}})
    end
end

exp_:push(V'exp_')
---a comma separated list of expressions
---@TODO error handling. insert `+ _listElement_ * err""` in the middle ?
exp_:push(seq(V(#exp_), T_",", true, 'list', 'exps'))
exp_.list_ = V(#exp_)
---@TODO add lhs_ check at some later point ?
do
    local _validLhs = {
        io = true,
        variable = true,
        indexed = true,
        assign = true,
        fun = true,
    }
    exp_.lhs_ = Cmt(false
        + fun_
        + V'ref_' --TODO add more
--        + V'group_'
--        + V'list_'
        ,
        function (_, _, lhs)
            if _validLhs[lhs.tag] then
                return true, lhs
            else
                return false, "Invalid Left hand side for assignement"  ---@TODO see whether and where it is possible to throw this error/warning
            end
        end
    )
end
exp_:push(V'lhs_' * T_"=" * V(#exp_ + 1) / nodeAssign + V(#exp_))
exp_.assign_ = V(#exp_)
---a juxtaposed sequence of lists and assignements
exp_:push(seq(V(#exp_), nil, true))
---semi-colon_can be used to break sequences.
exp_:push(seq(V(#exp_), T_";"))
exp_.seqs_ = V(#exp_)
exp_.seqs1_ = seq(V(#exp_ - 1), T_";", true)

--[[
---a comma separated list of expressions
---@TODO error handling. insert `+ _listElement_ * err""` in the middle ?
exp_.list_ = V'seq_' * (T_"," * V'seq_')^0 * T_","^-1 /
    Node(Node.isEmpty, 2, {Node.isEmpty, 1, {Node.empty}, {1}}, {{tag = 'list', [0] = 'exps'}})
---a juxtaposed sequence of lists
--- Cc(nil) to escape the pesky full match being returned when no capture occurs (nil is part of the capture but forgotten upon filling the table)
--- semicolon_can be used to break sequences, which doesn't do much, except with if and while blocks.
---@TODO try ^0. Remove Cc(nil) if useless.
exp_.seq_ = (V'exp_')^1 * Cc(nil) / Node(Node.isEmpty, 2, {Node.isEmpty, 1, {Node.empty}, {1}}, {{tag = 'seq', [0] = 'stats'}})
---@TODO add lhs_ check  at somelater point ?
exp_.assign_ = V'lhs' * T_"=" * V'rhs_' / nodeAssign + V'list_'
exp_.assignList_ = (V'assign_')^1 * Cc(nil) / Node(Node.isEmpty, 2, {Node.isEmpty, 1, {Node.empty}, {1}}, {{tag = 'seq', [0] = 'stats'}})
exp_.seqs_ = ((V'assignList_' + V'list_') * (T_";" * (V'assignList_' + V'list_'))^0 * semicolon_ + Cc(nil)) /
    Node(Node.isEmpty, 2, {Node.isEmpty, 1, {Node.empty}, {1}}, {{tag = 'seq', [0] = 'stats'}})
exp_.seqs1_ = V'list_' * (T_";" * V'list_')^0 * semicolon_ /
    Node(Node.isEmpty, 2, {Node.isEmpty, 1, {Node.empty}, {1}}, {{tag = 'seq', [0] = 'stats'}})
--]]

exp_ = P(exp_)

---@TODO : replace if with therefore. (after switching all statements to expressions). "therefore" is like "and" but with different prio yields the last truthy expression rather than the first falsy.
---@TODO  => is the purely logical, right associative, `imply` and is enough 
local stats_ = {'stats',
    ws_ = ws_,
    exp_ = exp_,
    ref_ = ref_,
    lhs_ = V'list_' + V'ref_',
    stat_ = --( V'block'
    ---@TODO put more sutff into lhs, such as `(a=b) = c` (meaning `a=b; a=c`) and `a < b = c` (meaning `if (a < b) {a = c} a` ). add !a = b and !!a = b ?
--        + V'lhs_' * T_"=" * V'exp_' / Node{tag = 'stat_assign', "lhs", "exp"}
        ---@TODO : implement a ternary operator instead
        ---(if)? <exp> ((therefore|otherwise) <exp>)* else <exp>
        ---where (therefore|otherwise) is right associative.
        ---relate it to promise style : 
            ---new a
            ---a.therefore().else().therefore() ... --(therefore|otherwise) chain
            ---a.else() --else
        --- <exp> ?: <exp> (, <exp>)* 
        --- <exp> ?: <exp> (, <exp>)* 
        --- <exp> ?: <exp> (, <exp>)* ; 
        ---@TODO ponder whether you want to do things like exp * ws^1 * exp, and give it a lower priority than that I guess
        --+ Rw_"if" * V'exp_' * V'stat_' * (Rw_"else" * V'stat_')^-1 / Node{tag = 'if', "cond", "stat_then", "stat_else"}
        --+ Rw_"while" * V'exp_' * V'stat_' / Node{tag = 'while', 'cond', 'stat'}
        -- + T_"@ =" * V'exp_' / Node{tag = 'stat_print', "exp"}
        --+ Rw_"return" * (V'exp_' + Cc(nil)) / Node{tag = 'return', "exp"}
        --+ V'exp_' ) 
        T_";"^-1 * (- T_";" + err"useless semi-colons are not allowed, you peasant!"),
    ---@TODO make/check ';' optional. (or maybe give it a meaning related to promise/chaining line of code in a sync/async manner ?)

    stats =  (V'stat_')^0 * Cc(nil) / Node(Node.isEmpty, 2, {Node.isEmpty, 1, {Node.empty}, {1}}, {{tag = 'seq', [0] = 'stats'}}),  -- Cc(nil) to escape the pesky full match being returned when no capture occurs. Nil is part of the capture but forgotten upon filling the table
    block = T_"{" * V'stats' * (T_"}" + err"block: missing brace"),
}
--stats_ = P(stats_)

--------------------------------------------------------------------------------
local successParsing = print

local filePatt =
    ws_ * (exp_)-- + stats_)
     * (-P(1) + err"file: syntax error.") --TODO better error msg
    -- * (-P(successParsing) / 0)
---@return AST
local function parse (input)
    return assert(filePatt:match(input))
end
--------------------------------------------------------------------------------
return parse