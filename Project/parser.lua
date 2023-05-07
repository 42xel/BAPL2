---@TODO vague floating idea : make code from AST. Put comments/annotations in AST
---enables pretty print, automatic documentation, helps introspection, debugging, reflexivity.
---helps changing syntax.
local pt = require "pt".pt
local lpeg = require "lpeg"

local Object = require"Object"
local Stack = require"Stack"
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

---@TODO optimisation (eg with constant addition/multiplication etc)
---@TODO OOP : use __name and __tostring for custom objects.
--------------------------------------------------------------------------------
---@alias AST table
--------------------------------------------------------------------------------

local MetaNode = Object{__name = "MetaNode"}
local Node = MetaNode{__name = "Node",tag = "void", _sel = 1}
--[[
a function generator which generates node functions, used to parse captures as nodes of the ast
***
### Usage: 
look at examples

    Node()  -- generates an empty node.
    Node{tag = "tag", field1, field2}   -- generates based on a template
    Node(predicate, number, {args1}, {args2})   -- uses a predicate on the numberth argument to chose another node function
    Node(number)    -- when 0 < n : chooses the numberth argument
    Node(number, args)  -- generate a function with the numberth argument as the starting node (as opposed to creating a new one)
***
#### TODO
- (after OO) incorporate lineCount. Do you really need them ?
- use meta programming
- : document usage and returned function
***
### Node Templates:
    {tag = "tag", [0] = vararg, field1, field2}
#### TODO
        more examples, with explanations
]]
function MetaNode:__call (t, n, argst1, argst2, ...)
    if type(t) == "table" then
        local sel = self._sel
        return function (...)
            local r = select(sel, {}, ...)
            for k, v in pairs(t) do
                local tk = type(k)
                if k == 0 then
                    r[v] = {select(1 + #t, ...)} --variable stuff of variable number.
                elseif tk == "number" then  --variable stuff, such as exp1, exp2, stat1, stat2 ...
                    r[v] = select(k, ...)
                elseif tk == "string" then  --constant stuff, such as tag
                    r[k] = v
                end
            end
            return r
        end
    elseif type(t) == "function" then
        return function (...)
            if t(select(n, ...)) then
                return Node[argst1](...)
            else
                return Node[argst2](...)
            end
        end
    elseif type(t) == "number" then
        self._sel = t + 1
        local r = self(n, argst1, argst2, ...)
        self._sel = 1
        return r
    elseif type(t) == "nil" then
        local sel = self._sel
        return function(...) return select(sel, {}, ...) end
    else
        error("MetaNode:__call() : first argument must be a table, a function or a number. Got "
            .. tostring(t)
            .. (type(t) == "userdata" and " . You might be using something undefined (_G shenanigans)." or ''))
    end
end
Node.empty = {tag = 'void'}
function MetaNode:__index(packedTable)
    if packedTable == nil then return print("packedTable nil") and Node.empty
    elseif type(packedTable) ~= 'table' then return print("packedTable not table") end

    self[packedTable] = self(table.unpack(packedTable))
    return self[packedTable]
end
function Node.isEmpty(self)
    return self == nil or self.tag == 'void'
end

local nodeNum = Node{tag = 'number', 'val'}

local nodeBinop = Node{tag = 'binop', 'exp1', 'op', 'exp2'}
---@TODO see whether isNodeEmpty is really necssary/helpful there.
local nodeFoldBinop = Node(Node.isEmpty, 2, {1}, {2, {tag = 'binop', 'exp1'}})
local nodeFoldBinopSuffix = Node{tag = 'binopSuffix', 'op', 'exp2'}
local nodeUnaryop = Node{tag = 'unaryop', 'op', 'exp'}    --local nodeUnaryop = nodeGenerator(isNodeEmpty, 2, {1}, {tag = "unaryop", "op", "e"})

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
--local function inc(x)
--    return x + 1
--end

--spaces
---@TODO : store lineCount and lastLineStart inside or around the AST for reporting and syntax highlight
---@TODO : use doo/doone/doon't for block comments ?

local blockComment = "#{" * (P(1) - "#}")^0 * "#}"
local lineComment = "#" * (P(1) - "\n")^0
local comment = blockComment + lineComment
local newLine = '\n' --  * Cg(Cb("lineCount") / inc, "lineCount") * Cg(Cp(), "lastLineStart")
---@TODO put spaces inside the grammar
--local ws = V"ws"
--local ws = V"ws_"
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

---@TODO alleviate the need for reserved words (stil have special words)
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
local var = ID / Node{tag = "variable", "var"}

--------------------------------------------------------------------------------
--elaborate patterns utils

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

--------------------------------------------------------------------------------
--expressions and statements

--for assignements :
--local lhs = V'lhs'
--local rhs = V'rhs'

local _parenNames = {
    ["("] = "parentheses",
    ["["] = "bracket",
    ["{"] = "brace",
}
local function paren_ (op, patt, cl, pattName, bname)
    bname = bname or _parenNames[op]
    return T_(op) * patt * (T_(cl) + err(pattName .. ": missing closing " .. bname .. "."))
end
local _parenNames = {
    ["("] = "parentheses",
    ["["] = "brackets",
    ["{"] = "braces",
}


local brackExp_ = paren_("[", V'exp_',"]", "expression")
local parenExp_ = paren_("(", V'exp_',")", "expression")
---@TODO make a function for parenthese, that maybe also checks for extra closing ones ?
local ref_ = Cf(var * V'ws_' * paren_("[", V'exp_',"]", "Array index")^0, Node{tag = 'indexed', 'exp_ref', 'exp_index'})

---@TODO : make every statement expression.
-- a list of constructs useable to build expression, from highest to lowest priority
local exp_ = Stack{'exp_',
    ws_ = ws_,
    ref_ = ref_,
    lhs_ = V'ref_',
    numeral * V'ws_' + ref_ + paren_("(", V'exp_',")", "primary"), --primary
}

--array creation
---@TODO remove useless keyword or switch to manual memory management
--exp_._indexChain_ = infixOpCaptureRightAssoc(Cc(nil), V'_indexChain_', brackExp_ + T_"=" * V'exp_', 'new')
--- `brackExp_ + T_"=" * V'exp_'`  : it's ok, T_"=" * V'exp_' is necessarilly last.
---I put it here, it makes more sense than in assign cause it's only useable for initialization, later on writing ̀`myTab = 0` won't fill myTab, it will just set myTab to 0.
exp_:push(Cfr(Rw_"new" * paren_("[", V'exp_',"]", "new Array", "bracket")^1 * (T_"=" * V'exp_' + Cc(nil)),
    Node{tag='new', 'exp_size', 'exp_default'}) + V(#exp_))
---litteral form
exp_:push(paren_("{", V'exp_' * (T_"," * V'exp_')^0 + Cc(nil), "}", "litteral Array")
    / Node{tag = 'new', [0] = 'values'} + V(#exp_))

exp_:push(infixOpCaptureRightAssoc(C"^" * V'ws_', V(#exp_+1),  V(#exp_))) --power
exp_:push(unaryOpCapture(C(S"+-"), V(#exp_ + 1), V(#exp_))) --unary +-
exp_:push(infixOpCapture(C(S"*/%") * V'ws_', V(#exp_))) --multiplication
exp_:push(infixOpCapture(C(S"+-") * V'ws_', V(#exp_))) --addition
---comparisons create booleans, so having logical operators of lower precedence alow to combine them wihout parentheses makes sense.
exp_:push(infixChainCapture(C(S"<>" * P"="^-1 + S"!=" * "=") * V'ws_', V(#exp_), 'compChain')) --comparison
exp_:push(unaryOpCapture(C"!", V(#exp_ + 1), V(#exp_)))    --unary not.
exp_:push(infixOpCaptureRightAssoc(C"&&" * V'ws_', V(#exp_ + 1), V(#exp_), 'conjunction'))    --binary and.
exp_:push(infixOpCaptureRightAssoc(C"||" * V'ws_', V(#exp_ + 1), V(#exp_), 'disjunction'))    --binary or.
--BEWARE : + V(#exp_) must be at the end
---assignement
exp_:push(V'lhs_' * T_"=" * V'exp_' / Node{tag = "assign", "lhs", "exp"}
    + V(#exp_))

exp_.exp_ = V(#exp_)
--setmetatable(exp_, nil)
exp_ = P(exp_)

---@TODO : use infixOpCaptureRightAssoc and modify nodeAssign so as to be able to chain assignement (C/C++/js/... style). Issue : emptying the stack if the value is not used
---@TODO : replace if with therefore. (after switching all statements to expressions). "therefore" is like "and" but with different prio yields the last truthy expression rather than the first falsy.
---@TODO  => is the purely logical, right associative, `imply` and is enough 
local stats_ = {'stats',
    ws_ = ws_,
    exp_ = exp_,
    ref_ = ref_,
    lhs_ = V'ref_',
    stat_ = (V'block'
    ---@TODO put more sutff into lhs, such as `(a=b) = c` (meaning `a=b; a=c`) and `a < b = c` (meaning `if (a < b) {a = c} a` ). add !a = b and !!a = b ?
--        + V'lhs_' * T_"=" * V'exp_' / Node{tag = "stat_assign", "lhs", "exp"}
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
        + Rw_"if" * V'exp_' * V"stat_" * (Rw_"else" * V"stat_")^-1 / Node{tag = "if", "exp_cond", "stat_then", "stat_else"}
        + Rw_"while" * V'exp_' * V"stat_" / Node{tag = "while", "exp_cond", "stat_"}
        + T_"@" * V'exp_' / Node{tag = "print", "exp"}
        + Rw_"return" * (V'exp_' + Cc(nil)) / Node{tag = "return", "exp"}
        + V'exp_'
        ) * T_";"^-1 * (- T_";" + err"useless semi-colons are not allowed, you peasant!"),
    ---@TODO make/check ';' optional. (or maybe give it a meaning related to promise/chaining line of code in a sync/async manner ?)
    stats =  (V'stat_')^0 * Cc(nil) / Node(Node.isEmpty, 2, {Node.isEmpty, 1, {Node.empty}, {1}}, {{tag = "seq", [0] = 'stats'}}),  -- Cc(nil) to escape the pesky full match being returned when no capture occurs. Nil is part of the capture but forgottent upon filling the table
    block = T_"{" * V'stats' * (T_"}" + err"block: missing brace"),
}
stats_ = P(stats_)

--------------------------------------------------------------------------------
local successParsing = print

local filePatt =
    ws_ * (stats_ + exp_)
     * (-P(1) + err"file: syntax error.") --TODO better error msg
     * (-P(successParsing) / 0)
---@return AST
local function parse (input)
    return filePatt:match(input)
end
--------------------------------------------------------------------------------
return parse