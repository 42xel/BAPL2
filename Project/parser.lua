local lpeg = require "lpeg"
local pt = require "pt".pt
local utils = require "utils"

local _Gmeta = getmetatable(_G)
setmetatable(_G, {
    __index = setmetatable(lpeg, {
        __index = function (self, key)
            self[key] = self.V(key)
            return self[key]
        end,
    })
})   --what could possibly go wrong ?

--------------------------------------------------------------------------------
--utils

-- a function generator which generates node functions, used to parse captures as nodes of the ast
-- Usage: look at examples
-- nodeGenerator{tag = "tag", field1, field2}   : generates based on a template
-- nodeGenerator(predicate, number, {args1}, {args2})   : uses a predicate on the numberth argument to chose another node function
-- nodeGenerator(number)    : chooses the numberth argument
-- nodeGenerator(number, args)  : generate a function with the numberth argument as the starting node (as opposed to creating a new one)
--TODO : document usage and returned function
local nodeGenerator = setmetatable({_sel = 1}, {
    __call = function (self, t, n, argst1, argst2, ...)
        if type(t) == "table" then
            local sel = self._sel
            return function (...)
                print("sel", pt(sel))
                local r = select(sel, {}, ...)
                for k, v in pairs(t) do
                    local tk = type(k)
                    if tk == "number" then  --variable stuff, such as e1, e2, st1, st2 ...
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
            return function(...) return select(sel, {}, ...) end--???
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
local emptyNode = {tag = "void"}

--[[old nodes examples
local function nodeSeq(st1, st2)
    return isNodeEmpty(st2) and st1 or {tag = "seq", st1 = st1, st2 = st2}
end
local function nodeBinop(a, op, b)
    return {
        tag = "binop",
        op = op,
        e1 = a,
        e2 = b,
    }
end
--all in all, 30 well invested lines of codes in the function generator to save 10 uses of the 'function' keywords, and give the linter a harder time.
--]]
--TODO : add comments?
--TODO see whether isNodeEmpty is really necssary there.

local nodeSeq = nodeGenerator(isNodeEmpty, 2, {1}, {{tag = "seq", "st1", "st2"}})
local nodeRet = nodeGenerator{tag = "return", "exp"}
local nodePrint = nodeGenerator{tag = "print", "exp"}
local nodeAssign = nodeGenerator{tag = "assign", "id", "exp"}
local nodeNum = nodeGenerator{tag = "number", "val"}
local nodeVar = nodeGenerator{tag = "variable", "var"}

local nodeBinop = nodeGenerator{tag = "binop", "e1", "op", "e2"}
local nodeFoldBinop = nodeGenerator(isNodeEmpty, 2, {1}, {2, {tag = "binop", "e1"}})
local foldSuffBin = nodeGenerator{tag = "binopSuffix", "op", "e2"}
local fold1Unary = nodeGenerator{tag = "unaryop", "op", "e"}    --local fold1Unary = nodeGenerator(isNodeEmpty, 2, {1}, {tag = "unaryop", "op", "e"})

--TODO : rework comparisons to do something smarter (and do it later) to compute middle terms only once.
--TODO For later, when I ll know flow, andmemory alloted to interpreter
--TODO Or maybe when I ll use leftValues
local function foldCompChain(t)
    --a < b < c will ultimatelybe transformed into (a<b) and (b<c)
    if #t == 1 then return t[1] end
    local r = {
        tag = "varop",
        eStack = Stack{},
        clause = "conjonction",
    }
    for i = 2, #t, 2 do
        r.eStack:push(nodeBinop(t[i-1], t[i], t[i+1]))
        --Sizeable issue, in a < b < c, expression b is duplicated.
    end
    return r
end

--------------------------------------------------------------------------------
--elementary patterns
local ws = S' \t\n'    --we might need ws or ws^1 in some places
local ws_ = ws^0

local comma = S'.'

local Assign_ = '=' * ws_

local digit = R'09'
local digits = digit^0
local hexdigit = R('09', 'af', 'AF')
local hexdigits = hexdigit^0
local function numeralCapture(digit, comma)
    return (digit^0 * (comma * digit^0)^-1) - ((comma+'')*-(digit+comma))
end
--I might want to have coding numeral stuck to variable identifiers or very special operators, so no spaces at the end.
local numeral = ('0' * lpeg.S'xX' * numeralCapture(hexdigit, comma) + numeralCapture(digit, comma) * (S'eE' * digit^1)^-1) / tonumber / nodeNum

local alpha = R('az', 'AZ')
local alphanum = alpha+digit

--no spaces, potentially, things like field access need to be stuck to the ID
--the possibility of no spaces before seems more important though
local ID = C((alpha + '_') * (alphanum + '_')^0)
local var = ID / nodeVar

local OP_ = '(' * ws_
local CP_ = ')' * ws_
local OB_ = '{' * ws_
local CB_ = '}' * ws_
local SC_ = ';' * ws_

local ret_ = "return" * ws_
local printStat_ = '@' * ws_



--------------------------------------------------------------------------------

local function infixOpCapture(opPatt, abovePattern)
    return lpeg.Cf(abovePattern * (opPatt * abovePattern / foldSuffBin)^0, nodeFoldBinop) 
end
local function infixOpCaptureRightAssoc(opPatt, selfPattern, abovePattern)  --set self to above to have a non asociative binary op.
    --return lpeg.Cg(abovePattern, 'fst') * lpeg.Cg(lpeg.Cb('fst') * (opPatt * selfPattern / foldSuffBin) / nodeFoldBinop, 'fst')^-1 * lpeg.Cb('fst') --overkill
    return abovePattern * (opPatt * selfPattern / foldSuffBin)^-1 / nodeFoldBinop
end
local function unaryOpCapture(opPatt, selfPattern, abovePattern)  --allows chaining. set self to above to disallow
    return abovePattern + opPatt * ws_ * abovePattern / fold1Unary + opPatt * ws^1 * selfPattern / fold1Unary --not allowing ++ , -- or +- , but allowing - -
end
local function infixCompChainCapture(opPatt, abovePattern)
    return lpeg.Ct(abovePattern * (opPatt * abovePattern)^0) / foldCompChain
end

-- a list of constructs useable to build expression, from highest to lowest priorityst+2)))
local expGrammar = Stack{"stats",
    (numeral + var) * ws_ + OP_ * exp * CP_, --primary
}
expGrammar:push(unaryOpCapture(lpeg.C(lpeg.S'+-'), V(#expGrammar + 1), V(#expGrammar))) --unary +-
expGrammar:push(infixOpCaptureRightAssoc(lpeg.C(lpeg.S'^') * ws_, V(#expGrammar+1),  V(#expGrammar))) --power
expGrammar:push(infixOpCapture(lpeg.C(lpeg.S'*/%') * ws_, V(#expGrammar))) --multiplication
expGrammar:push(infixOpCapture(lpeg.C(lpeg.S'+-') * ws_, V(#expGrammar))) --addition
expGrammar:push(infixCompChainCapture(lpeg.C(lpeg.S'<>' * lpeg.P'='^-1 + lpeg.S'!=' * '=') * ws_, V(#expGrammar))) --comparison

expGrammar.exp = V(#expGrammar)
expGrammar.stat = block
    + ID * ws_ * Assign_ * exp / nodeAssign
    + ret_ * exp / nodeRet
    + printStat_ * exp / nodePrint
    + lpeg.Cc(emptyNode)
expGrammar.stats = stat * (SC_ * stats)^-1 / nodeSeq
expGrammar.block = OB_ * stats * CB_

expGrammar = ws_ * lpeg.P(expGrammar) * -1

local function parse (input)
    return expGrammar:match(input)
end

--TODO : use infixOpCaptureRightAssoc and modify nodeAssign so as to be able to chain assignement (C/C++/js/.. style). Issue : emptying the stack if the value is not used
--------------------------------------------------------------------------------
setmetatable(_G, _Gmeta)
return parse