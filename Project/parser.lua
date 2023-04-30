local pt = require "pt".pt
local lpeg = require "lpeg"

local Object = require"Object"
local Stack = require"Stack"
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

---@TODO optimisation (eg with constant addition/multiplication etc)
---@TODO OOP : use __name and __tostring for custom objects.
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
Node.empty = {tag = "void"}
function MetaNode:__index(packedTable)
    if packedTable == nil then return Node.empty
    elseif type(packedTable) ~= "table" then return end

    self[packedTable] = self(table.unpack(packedTable))
    return self[packedTable]
end
function Node.isEmpty(self)
    return self == nil or self.tag == "void"
end

local nodeNum = Node{tag = "number", "val"}

local nodeBinop = Node{tag = "binop", "exp1", "op", "exp2"}
---@TODO see whether isNodeEmpty is really necssary/helpful there.
local nodeFoldBinop = Node(Node.isEmpty, 2, {1}, {2, {tag = "binop", "exp1"}})
local nodeFoldBinopSuffix = Node{tag = "binopSuffix", "op", "exp2"}
local nodeUnaryop = Node{tag = "unaryop", "op", "exp"}    --local nodeUnaryop = nodeGenerator(isNodeEmpty, 2, {1}, {tag = "unaryop", "op", "e"})

--------------------------------------------------------------------------------
--elementary patterns

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
--set token generator
local function Ts_(tokens)
    return S(tokens) * ws_
end

---@TODO make msg an object ??
---@TODO use a patt argument for nested/chained errors ??
local function err(msg)
    return Cmt('', function (subject, p)
        local lineStart = 1
        local _, lineNumber = subject:sub(1, p):gsub("()\n", function (pos) lineStart = pos end)
            io.stderr:write(string.format("error in <input>:%d:%d\t", lineNumber + 1, p - lineStart + 1))    --TODO use filename
            io.stderr:write(msg .. '\n')
            os.exit()
    end)
end

local comma = S'.'

--local digit = R'09'
local digit = locale.digit
local digits = digit^0
--local xdigit = R('09', 'af', 'AF')
local xdigit = locale.xdigit
local xdigits = xdigit^0
---@TODO if no other pattern starts with a digit, or if numeral is the last choice among them, loosen this pattern and raise a more informative error
local function numeralCapture(digit, comma)
    return (digit^0 * (comma * digit^0)^-1) - ((comma+'')*-(digit+comma))
end
--I might want to have coding numeral stuck to variable identifiers or very special operators, so no spaces at the end.
local numeral = ('0' * S'xX' * numeralCapture(xdigit, comma) + numeralCapture(digit, comma) * (S'eE' * digit^1)^-1) / tonumber / nodeNum

local alpha = locale.alpha
--local alpha = R('az', 'AZ')
--local alnum = alpha+digit
local alnum = locale.alnum

local Rw_ = setmetatable({
    "return",
    "if",
    "else",
    "therefore",
    "goto",
    "while",
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

---@TODO : make every statement expression.
-- a list of constructs useable to build expression, from highest to lowest priority
local exp_ = Stack{'exp',
    (numeral + var) * ws_ + T_"(" * V'exp' * (T_")" + err"primary: missing parentheses"), --primary
}
exp_:push(infixOpCaptureRightAssoc(C"^" * ws_, V(#exp_+1),  V(#exp_))) --power
exp_:push(unaryOpCapture(C(S"+-"), V(#exp_ + 1), V(#exp_))) --unary +-
exp_:push(infixOpCapture(C(S"*/%") * ws_, V(#exp_))) --multiplication
exp_:push(infixOpCapture(C(S"+-") * ws_, V(#exp_))) --addition
---comparisons create booleans, so having logical operators of lower precedence alow to combine them wihout parentheses makes sense.
exp_:push(infixChainCapture(C(S"<>" * P"="^-1 + S"!=" * "=") * ws_, V(#exp_), 'compChain')) --comparison
exp_:push(unaryOpCapture(C"!", V(#exp_ + 1), V(#exp_)))    --unary not.
exp_:push(infixOpCaptureRightAssoc(C"&&" * ws_, V(#exp_ + 1), V(#exp_), 'conjunction'))    --binary and.
exp_:push(infixOpCaptureRightAssoc(C"||" * ws_, V(#exp_ + 1), V(#exp_), 'disjunction'))    --binary or.

exp_.exp = V(#exp_)
--setmetatable(exp_, nil)
exp_ = P(exp_)

---@TODO : use infixOpCaptureRightAssoc and modify nodeAssign so as to be able to chain assignement (C/C++/js/... style). Issue : emptying the stack if the value is not used
---@TODO : replace if with therefore. (after switching all statements to expressions). "therefore" is like "and" but with different prio yields the last truthy expression rather than the first falsy.
---@TODO : else is
---@TODO  => is the purely logical, right associative, `imply` and is enough 
local stats_ = {'stats',
    stat = (V'block'
        + ID * ws_ * T_"=" * exp_ / Node{tag = "assign", "id", "exp"}
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
        ---@TODO ponder whether you want to od things like exp * ws^1 * exp, and give it a lower priority than that I guess
        + Rw_"if" * exp_ * V"stat" * (Rw_"else" * V"stat")^-1 / Node{tag = "if", "exp_cond", "stat_then", "stat_else"}
        + Rw_"while" * exp_ * V"stat" / Node{tag = "while", "exp_cond", "stat"}
        + T_"@" * exp_ / Node{tag = "print", "exp"}
        + Rw_"return" * exp_ / Node{tag = "return", "exp"}
        ) * T_";"^-1 * (- T_";" + err"useless semi-colons are not allowed, you peasant!"),
    ---@TODO make/check ';' optional. (or maybe give it a meaning related to promise/chaining line of code in a sync/async manner ?)
    stats = Cc'seq' * V'stat'^0 / Node(Node.isEmpty, 3, {Node.isEmpty, 2, {Node.empty}, {2}}, {{"tag", [0] = "stats"}}),  -- no constant tag to escape the annoying full pattern returned when no capture occurs
    block = T_"{" * V'stats' * (T_"}" + err"block: missing brace"),
}
stats_ = P(stats_)

--------------------------------------------------------------------------------

local filePatt =
    --Cg(Cc(1), "lineCount") * Cg(Cc(1), "lineStart") * 
    ws_ * stats_
     * (-1 + err"file: syntax error.") --TODO better error msg
local function parse (input)
    return filePatt:match(input)
end
--------------------------------------------------------------------------------
return parse