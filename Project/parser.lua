local pt = require "pt".pt
local lpeg = require "lpeg"

local utils = require "utils"

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

--TODO optimisation (eg with constant addition/multiplication etc)
--------------------------------------------------------------------------------
--utils

---@TODO developp and clean up the OO aspect of Node. Maybe replace nodeGenerator by Node
local Node = Object{tag = "void"}
local emptyNode = Node:new()
--[[
a function generator which generates node functions, used to parse captures as nodes of the ast
***
### Usage: 
look at examples

    nodeGenerator()  -- generates an empty node.
    nodeGenerator{tag = "tag", field1, field2}   -- generates based on a template
    nodeGenerator(predicate, number, {args1}, {args2})   -- uses a predicate on the numberth argument to chose another node function
    nodeGenerator(number)    -- when 0 < n : chooses the numberth argument
    nodeGenerator(number, args)  -- generate a function with the numberth argument as the starting node (as opposed to creating a new one)
***
#### TODO
- (after OO) incorporate lineCount. Do you really need them ?
- use meta programming
- : document usage and returned function
***
### template:
    {tag = "tag", [0] = vararg, field1, field2}
]]
local nodeGenerator = setmetatable({_sel = 1}, {
    __call = function (self, t, n, argst1, argst2, ...)
        if type(t) == "table" then
            local sel = self._sel
            return function (...)
                local r = select(sel, Node:new(), ...)
                for k, v in pairs(t) do
                    local tk = type(k)
                    if k == 0 then
                        r[v] = {select(1, ...)} --variable stuff of variable number.
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
                    return self[argst1](...)
                else
                    return self[argst2](...)
                end
            end
        elseif type(t) == "number" then
            self._sel = t + 1
            local r = self(n, argst1, argst2, ...)
            self._sel = 1
            return r
        elseif type(t) == "nil" then
            local sel = self._sel
            return function(...) return select(sel, Node:new(), ...) end--???
        else
            error("nodeGenerator() : first argument must be a table, a function or a number. Got "
                .. tostring(t)
                .. (type(t) == "userdata" and " . You might be using something undefined (_G shenanigans)." or ''))
        end
    end,
    __index = function(self, packedTable)
        self[packedTable] = self(table.unpack(packedTable))
        return self[packedTable]
    end,
})

--------------------------------------------------------------------------------
--node capture functions

local function isNodeEmpty(n)
    return n == nil or n.tag == "void"
end

--[[old nodes examples
local function nodeSeq(stat1, stat2)
    return isNodeEmpty(stat2) and stat1 or {tag = "seq", stat1 = stat1, stat2 = stat2}
end
local function nodeBinop(a, op, b)
    return {
        tag = "binop",
        op = op,
        exp1 = a,
        exp2 = b,
    }
end
--all in all, 30 well invested lines of codes in the function generator to save 10 uses of the 'function' keywords, and give the linter a harder time.
--]]
--TODO : add comments?
--TODO see whether isNodeEmpty is really necssary/helpful there.

local nodeSeq = nodeGenerator(isNodeEmpty, 2, {1}, {{tag = "seq", [0] = "stats"}})
local nodeRet = nodeGenerator{tag = "return", "exp"}
local nodePrint = nodeGenerator{tag = "print", "exp"}
local nodeAssign = nodeGenerator{tag = "assign", "id", "exp"}
local nodeNum = nodeGenerator{tag = "number", "val"}
local nodeVar = nodeGenerator{tag = "variable", "var"}
local nodeIf = nodeGenerator{tag = "if", "exp_cond", "stat_then"}

local nodeBinop = nodeGenerator{tag = "binop", "exp1", "op", "exp2"}
local nodeFoldBinop = nodeGenerator(isNodeEmpty, 2, {1}, {2, {tag = "binop", "exp1"}})
local nodeFoldBinopSuffix = nodeGenerator{tag = "binopSuffix", "op", "exp2"}
local nodeUnaryop = nodeGenerator{tag = "unaryop", "op", "exp"}    --local nodeUnaryop = nodeGenerator(isNodeEmpty, 2, {1}, {tag = "unaryop", "op", "e"})

--Sizeable issue, in a < b < c, expression b is duplicated.
--TODO : rework comparisons to do something smarter (and do it later) to compute middle terms only once.
--TODO For later, when I ll know flow, andmemory alloted to interpreter
--TODO Or maybe when I ll use leftValues
local function foldCompChain(t)
    --a < b < c will ultimately be transformed into (a<b) and (b<c)
    if #t == 1 then return t[1] end
    local r = Node:new{
        tag = "varop",
        eStack = Stack{},
        clause = "conjonction",
    }
    for i = 2, #t, 2 do
        r.eStack:push(nodeBinop(t[i-1], t[i], t[i+1]))
    end
    return r
end

--------------------------------------------------------------------------------
--elementary patterns

local locale = lpeg.locale()
local function inc(x)
    return x + 1
end

--spaces
--TODO : store lineCount and lastLineStart inside or around the AST for reporting and syntax highlight

local blockComment = "#{" * (P(1) - "#}")^0 * "#}"
local lineComment = "#" * (P(1) - "\n")^0
local comment = blockComment + lineComment
local newLine = '\n' --  * Cg(Cb("lineCount") / inc, "lineCount") * Cg(Cp(), "lastLineStart")
--TODO put spaces inside the grammar
--local ws = V"ws"
--local ws = V"ws_"
local ws = newLine + locale.space + comment    --we might need ws or ws^1 in some places
local ws_ = ws^0

--TODO think about what a token is and isn''t, and about spacing (operators probably aren''t token)
--token generator
local function T_(token)
    return token * ws_
end
--set token generator
local function Ts_(tokens)
    return S(tokens) * ws_
end


--TODO make msg an object ??
--TODO use a patt argument for nested/chained errors ??
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
--TODO if no other pattern starts with a digit, or if numeral is the last choice among them, loosen this pattern and raise a more informative error
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
    "elseif",
    "therefore",
    "goto",
},{__call = function(self, word)
    return self[word]
end,
    __index = function(self, key)
        error("Not a reserved word: " .. key, 3)
    end,
})
--local Rw = P(false)
for _, w in ipairs(Rw_) do
    Rw_[w] = w * - alnum * ws_
--    Rw = Rw + w
end
--Rw = Rw * - alnum

--no spaces, potentially, things like field access need to be stuck to the ID
--the possibility of no spaces before seems more important though
local ID = Cmt((alpha + '_') * (alnum + '_')^0, function (_, p, w)
    return rawget(Rw_, w) == nil, w
end)
local var = ID / nodeVar

--------------------------------------------------------------------------------
--elaborate patterns

local function infixOpCapture(opPatt, abovePattern)
    return Cf(abovePattern * (opPatt * abovePattern / nodeFoldBinopSuffix)^0, nodeFoldBinop) 
end
local function infixOpCaptureRightAssoc(opPatt, selfPattern, abovePattern)  --set self to above to have a non asociative binary op.
    --return Cg(abovePattern, '_') * Cg(Cb('_') * (opPatt * selfPattern / nodeFoldBinopSuffix) / nodeFoldBinop, '_')^-1 * Cb('_') --overkill.
    return abovePattern * (opPatt * selfPattern / nodeFoldBinopSuffix)^-1 / nodeFoldBinop
end
local function unaryOpCapture(opPatt, selfPattern, abovePattern)  --allows chaining. set self to above to disallow
    return abovePattern + opPatt * ws_ * abovePattern / nodeUnaryop + opPatt * ws^1 * selfPattern / nodeUnaryop --not allowing ++ , -- or +- , but allowing - -
end
local function infixCompChainCapture(opPatt, abovePattern)
    return Ct(abovePattern * (opPatt * abovePattern)^0) / foldCompChain
end

--TODO : make every statement expression.
-- a list of constructs useable to build expression, from highest to lowest priorityst+2)))
local exp_ = Stack{'exp',
    (numeral + var) * ws_ + T_"(" * V'exp' * (T_")" + err"primary: missing parentheses"), --primary
}
exp_:push(infixOpCaptureRightAssoc(C(S'^') * ws_, V(#exp_+1),  V(#exp_))) --power
exp_:push(unaryOpCapture(C(S'+-'), V(#exp_ + 1), V(#exp_))) --unary +-
exp_:push(infixOpCapture(C(S'*/%') * ws_, V(#exp_))) --multiplication
exp_:push(infixOpCapture(C(S'+-') * ws_, V(#exp_))) --addition
exp_:push(infixCompChainCapture(C(S'<>' * P'='^-1 + S'!=' * '=') * ws_, V(#exp_))) --comparison
--TODO : ponder and discuss priority. My idea : logical operator => very low prio.
--TODO : for example, comparisons create booleans, so having logical operators of lower precedence alow to combine them wihout parentheses.
exp_:push(unaryOpCapture(C'!', V(#exp_ + 1), V(#exp_)))    --unary not.

exp_.exp = V(#exp_)

exp_ = P(exp_)

--TODO : use infixOpCaptureRightAssoc and modify nodeAssign so as to be able to chain assignement (C/C++/js/... style). Issue : emptying the stack if the value is not used
--TODO : replace if with therefore. (after switching all statements to expressions)
local stats_ = {'stats',
    stat = V'block'
        + ID * ws_ * T_"=" * exp_ / nodeAssign
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
        + Rw_"if" * exp_ * V"stat" / nodeIf-- * (Rw_"else" * V"stat")^-1 / nodeIf
        + T_'@' * exp_ / nodePrint
        + Rw_"return" * exp_ / nodeRet,
    ---@TODO make/check ';' optional. (or maybe give it a meaning related to promise/chaining line of code in a sync/async manner ?)
    stats = ((T_';'^0 * (V'stat') * T_';'^0)^0) / nodeSeq,
    block = T_'{' * V'stats' * (T_'}' + err"block: missing brace"),
}
stats_ = P(stats_)

local filePatt = 
    --Cg(Cc(1), "lineCount") * Cg(Cc(1), "lineStart") * 
    ws_ * stats_
     * (-1 + err"file: syntax error.") --TODO better error msg
local function parse (input)
    return filePatt:match(input)
end
--------------------------------------------------------------------------------
return parse