---@TODO vague floating idea : make code from AST. Put comments/annotations in AST
---enables pretty print, automatic documentation, helps introspection, debugging, reflexivity.
---helps transpiling while changing syntax.
local pt = require "pt".pt
local lpeg = require "lpeg"

local Stack = require"Stack"
local Node = require"ASTNode"

local nodeNum = Node.nodeNum
local nodeBinop = Node.nodeBinop
local nodeFoldBinop = Node.nodeFoldBinop
local nodeFoldBinopSuffix = Node.nodeFoldBinopSuffix
local nodeUnaryop = Node.nodeUnaryop
local nodeIndexed = Node.indexed
local nodeAssign = Node.nodeAssign
local nodeBlock = Node.nodeBlock
local nodeReturn = Node.nodeReturn
--local nodeFunLHSparams = Node.funLHSparams
local nodeFun = Node.fun

require "utils"

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

local newLine = '\n' --  * Cg(Cb("lineCount") / inc, "lineCount") * Cg(Cp(), "lastLineStart")

local ws = newLine + locale.space    --we might need ws or ws^1 in some places
local ws_ = ws^0

--token generator
local function T_(token)
    return token * ws_
end
----set token generator
--local function Ts_(tokens)
--    return S(tokens) * ws_
--end

---@TODO ponder whether they should be, empty statements, list of statements, etc. Probably still no.
--local semicolon_ = T_";"^-1 * (- T_";" + err"useless semi-colons are not allowed, you peasant!")
--local semicolons_ = T_";"^0


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
---@TODO : use doo/doone/doon't for blocks and comment block ? Answer : no, it's moronic and conflicts violently with getting rid of keywords, unless you go full functional curry, which I won't.

---@TODO alleviate the need for reserved words (still have special words)
local Rw_ = setmetatable({
    "break",
    "return",
    "if",
    "else",
--    "therefore",
--    "goto",
    "while",
    "new",
--    "Array",    ---@TODO (after function and OO) : alternative way to declare Arrays, to eventually replace `new` keyword
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
---@TODO add `?` prefix meaning in the instance call environment, containing the arguments of a function
local varPrefix = "~" * Cc(0)  --global
    + C(P"?"^1) / function (s) return -#s end   --local dynamic
    * C(P"."^1) / string.len                    --local lexical
local var = (varPrefix * (V'ID' + C"") --potentially no var : referring to the context itself
    + Cc(nil) * V'ID') --no prefix : implicit (to be deduced by the compiler)
    / Node{tag = 'variable', 'prefix', 'var'}
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
---expressions and statements"."

local _parenNames = {
    ["("] = "parentheses",
    ["["] = "bracket",
    ["{"] = "brace",
}
---@TODO also checks for extra closing ones and relevant error message ? (maybe not here).
local function paren_ (op, patt, cl, pattName, bname)
    bname = bname or _parenNames[op]
    return T_(op) * patt * (T_(cl) + err(pattName .. ": missing closing " .. bname .. "."))
end


---@TODO : sort priority between formula, index and functions :
--a + b[] => a + (b[])
--b

-- We build formula by listing subconstructs, from highest to lowest priority.
-- in my understanding, priority order is really only needed for pattern needing disambiguation, such as infix op pattern.
local exp_ = Stack{'seqs_', --'exp_',
    ws_ = ws_,
    --indexedLHS_ = indexedLHS_,
    --refLHS_ = refLHS_,
    ID = ID,
    --an expressions or expression sequence delimited by parntheses
    pgroup_ = paren_("(", V'seqs_',")", "group"),
    group_ = V'pgroup_' + V'context_',
}

--------------------------------------------------------------------------------

--[[intricate patterns
- The easiest patterns to parse by far without encountering problem relative to priority or left recursivity are parenthesized patterns, preferably with a terminator differing from the opener. 
Obviously "exclusive" pairing is the best. A case of non exclusive pairing is the introduction of `#{ ... #}`, which is still very easy to handle. 
easier than `{# ... }#̀  would be anyway.
- next come prefix pattern. Since they are very upfront, we can easily distinguish them right away.
- next are infix patterns. Their priority needs to be adressed to specify semantics and they may cause some easily solved left recursivity issues.
- arguably last come suffix patterns. Much like the Reverse Polish Notation, they can be unambiguous without needing grouping. 
However, we know what we're reading the latest possible. 
On top of general efficiency consideration, a particular problem they raise in Peg is left recursion.

That's the main reasons why, after some back and forth, I made function prefix, so they don't clash with indexed pattern and make them generally easier to parse.
It raises the quesition of priority : what does
```#a[0]```
mean ? because the terse `#a,[0]` unambiguously means `(#a)[0]`, `#a[0]` must canonically mean `#(a[0])`

]]
_READ_INTRICATEPATTERNS = nil

--elaborate patterns utils for formula

exp_.prefixed_ = V'fun_' + V'if_' + V'while_' + V'break_' + V'return_'

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

--primary, aka first stage in formulas
exp_.primary_ =
    numeral * V'ws_'
    + var * V'ws_'
    + V'group_'
    + V'array_'     --to be used along lazy logical operator.
    ---@TODO add syntax (parer) for 3 + if cond exp. Currently, there is 3 + (if cond exp). Presumably by creating a prefixed_ pattern and adding it to the rhs of the 'elaborate patterns utils for formula'. Not really, needs to be on the last bit of formula, overall not PEG friendly and not worth it.
    ---@TODO (after the above, compiler) disallow 3 + (return), return return, etc.
    + V'if_'     --dubious here ?
    + V'while_'  --dubious here ?
    + V'break_'  --dubious here ?
    + V'return_' --dubious here ?
    --+ V'ref_'      --should'nt be needed here
exp_:push(V'primary_')

--using numbered pattern for flexibility concerning priorities, and named pattern for easier use elsqwhere.
--declaring the named pattern first to have more explicit error message from PEG (typically left recursivity)
exp_.indexed_ = Cf((var * V'ws_' + V'group_') * paren_("[", V'seqs_',"]", "Array index")^0, nodeIndexed)
exp_:push(V'indexed_' + V(#exp_))
--local indexedLHS_ = Cf(var * V'ws_' * paren_("[", V'seqs_',"]", "Array index")^0, nodeIndexed)
--exp_.indexed_ = indexedLHS_

--is a distinct ref_ from indexed_ really relevant and necessary ?
exp_.ref_ = T_"@" / Node{tag = 'io'}
    + V'indexed_'
--    + T_"@" / Node{tag = 'io'}
exp_:push((V'ref_' + V(#exp_)))
--[[
    function pattern
```
# a = exp
```
Means `a` is assigned to a function that yields `exp`.
The logic is the same as pointers syntax in C/C++ : a is assigned to the value such that `#a` evaluates to `exp`.

TODO anonymous function (and assignements statements) : `#= body` `#,param = body`
For now, the workaround `{#.=body}` should work, with caution given to the difference in scoping the block implies.

T_(C"#")^1 allows to fold but is itself never captured

as lhs : `## a = body` means `# a = {#.=body}` (and is kind of free) <br>
As rhs : `##a` means `#(#a)` (and is free)
]]
exp_.fun_ = T_"#" * V(#exp_ + 1) * V(#exp_ + 1)^-1 / nodeFun
exp_:push(exp_.fun_ + V(#exp_))

exp_:push(infixOpCaptureRightAssoc(C"^" * V'ws_', V(#exp_+1),  V(#exp_))) --power
exp_:push(unaryOpCapture(C(S"+-"), V(#exp_ + 1), V(#exp_))) --unary +-
exp_:push(infixOpCapture(C("//" + S"*/%") * V'ws_', V(#exp_))) --multiplication
exp_:push(infixOpCapture(C(S"+-") * V'ws_', V(#exp_))) --addition
---comparisons create booleans, so having logical operators of lower precedence allow to combine them wihout parentheses makes sense.
exp_:push(infixChainCapture(C(S"<>" * P"="^-1 + S"!=" * "=") * V'ws_', V(#exp_), 'compChain')) --comparison
exp_:push(unaryOpCapture(C"!", V(#exp_ + 1), V(#exp_)))    --unary not.
exp_:push(infixOpCaptureRightAssoc(C"&&" * V'ws_', V(#exp_ + 1), V(#exp_), 'conjunction'))    --binary and.
exp_:push(infixOpCaptureRightAssoc(C"||" * V'ws_', V(#exp_ + 1), V(#exp_), 'disjunction'))    --binary or.
exp_:push(infixOpCaptureRightAssoc(C"=>" * V'ws_', V(#exp_ + 1), V(#exp_), 'imply'))          --logical imply.
--[[
    Formulas are compounds of sticky subexpression, sticky meaning that they may stick to an other higher priority expresion.
    Operator priority is mostly only a thing with infix operators.
    Formula is the top level expression above such subexpression.
]]
exp_.formula_ = V(#exp_)

--------------------------------------------------------------------------------
---expressions

--array creation
---@TODO remove useless keyword or switch to manual memory management (to each new corresponds a delete, and any dynamic memory allocation needs a new)
--exp_._indexChain_ = infixOpCaptureRightAssoc(Cc(nil), V'_indexChain_', brackExp_ + T_"=" * V'exp_', 'new')
--- `brackExp_ + T_"=" * V'exp_'`  : it's ok, T_"=" * V'exp_' is necessarilly last.
---I put it here, it makes more sense than in assign cause it's only useable for initialization, later on writing ̀`myTab = 0` won't fill myTab, it will just set myTab to 0.
exp_.array_ = Cfr(Rw_"new" * paren_("[", V'seqs_', "]", "new Array")^1 * (T_"=" * V'exp_' + Cc(nil)),  --at least on result needed for size specifiers (seqs1_)
    Node{tag='new', 'size', 'default'})
---litteral Array
exp_.context_ = paren_("{", V'seqs_', "}", "block/litteral Array")
    / nodeBlock

-----code block. For now, you have to 
--exp_.block_ = paren_("{", V'rawList_', "}", "litteral Array")
--    / Node{tag = 'new', 'content'} -- [0] = 'values'}

local _errNoSC = err"BEWARE, you cannot separate condition, then and else with semi-colon"
--BEWARE, you can't separate condition, then and else with semicolon (that's intentional). You can use them clarify where the whole if stops though.
exp_.if_ = Rw_"if" * V'assign__' * (T_";" * _errNoSC + 0) * V'assign__' *
    ((T_";" * Rw_"else" * _errNoSC + Rw_"else") * V'assign__' * I'else')^-1 /
    Node{tag = 'if', "cond", "then", "else"}
exp_.while_ = Rw_"while" * V'assign__' * V'assign__' / Node{tag = 'while', 'cond', 'stat'}

exp_.return_ = Rw_"return" * (V'exp_' + Cc(nil)) / nodeReturn
--exp_.return_ = Rw_"return" * Cc(Node.empty)
exp_.break_ = Rw_"break" * (V'exp_' + Cc(nil)) / Node{tag = 'break', "exp"}

---A pure atomic expression in the sense of expressing a single value (compares with sequences and lists)
exp_.exp_ = (false
    + V'formula_' --contains parentheses group ok.    --+ V'array_' + V'context_'
)

--------------------------------------------------------------------------------
---sequences

---a function to create sequences patterns.
--- Cc(nil) to escape the pesky full match being returned when no capture occurs (nil is part of the capture but forgotten upon filling the table)
---@TODO error handling ? especially if sep non nil ?
local function seq(patt, sep, nonEmpty, tag, field)
    if sep then
        local r = patt * (sep * patt)^0
        if not nonEmpty then
            r = r + Cc(nil)
        end
        return r * sep^-1 /
        Node(Node.isEmpty, 2, {Node.isEmpty, 1, {Node.empty}, {1}}, {{tag = tag or 'seq', [0] = field or 'stats'}})
    else
        ---@TODO try ^0.
        ---@TODO Remove Cc(nil) if useless.
        return patt^(nonEmpty and 1 or 0) /
        Node(Node.isEmpty, 2, {Node.isEmpty, 1, {Node.empty}, {1}}, {{tag = tag or 'seq', [0] = field or 'stats'}})
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
    exp_.lhs_ = Cmt(false --TODO add more
        + V'fun_' -- + V'funLHS_'
        + V'ref_' --+ V'refLHS_'
        + V'pgroup_'
--[[
Ideal behavior (enables default arguments) :
R-evaluates rhs, L-evaluates lhs, lazy assign, clean the lhs part of the stack.

Pbtq : rhs is potentially computed at once, with dynamical size, so I can't just statically break asignemnt with list lhs into a list of assignements.
- Sol 1 : first evaluate lhs + new instruction combine with dynamic move (opInstruction mv_d) ? Probably no as I kind of want to evaluate rhs first in general. 

TODO how does it relates to function loading ?

]]
--        + V'list_'
        ,
        function (_, _, lhs)
            if _validLhs[lhs.tag] then
                --print("bla3", pt(lhs))
                return true, lhs
            else
                --print("bla4", lhs.tag, pt(lhs))
                return false, "Invalid Left hand side for assignement"  ---@TODO see whether and where it is possible to throw this error/warning
            end
        end
    )
end

exp_.assign_ = V'lhs_' * T_"=" * V(#exp_ + 1) / nodeAssign
exp_:push(exp_.assign_ + V(#exp_))
exp_.assign__ = V(#exp_)
---a juxtaposed sequence of lists and assignements. Comment out to make semi colons mandatory.
exp_:push(seq(V(#exp_), nil, true))


--local blockComment = ";{" * (P(1) - ";}")^0 * ";}"
---@param code string
local blockComment = P";{" * (Cmt(C((P"{")^0) * Cc"", function (code, i, brackets)
    return code:match(("}"):rep(1 + #brackets) .. '()', i)
 end)
    * V'ws_'
    + err"Block comment missing end bracket(s)")
local lineComment = ";" * (P(1) - "\n")^0 * V'ws_'-- * print
local comment = blockComment + lineComment

---semi-colon_can be used to break sequences.
exp_.seqs_ = seq(V(#exp_) + Cc(Node.empty) * V'ws_', comment)
exp_:push(exp_.seqs_)
--exp_.seqs_ = V(#exp_)
exp_.seqs1_ = seq(V(#exp_ - 1), comment, true)

--+ T_"@" / Node{tag = 'io'}
exp_ = P(exp_)

--------------------------------------------------------------------------------
local successParsing = print

local filePatt =
    ws_ * (exp_)-- + stats_)
     * (-P(1) + err"file: syntax error.") --TODO better error msg
    -- * (-P(successParsing) / 0)
---@return AST
local function parse (input)
    local r = filePatt:match(input)
    assert(r, "Parsing failed:\tunknown reason :'(")
    return r-- nodeReturn(nodeBlock(r))
end
--------------------------------------------------------------------------------
return parse