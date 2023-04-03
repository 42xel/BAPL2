local pt = require "pt".pt
local lpeg = require "lpeg"
local utils = require "utils"

--------------------------------------------------------------------------------
local function isNodeEmpty(n)
    return n == nil or n.tag == "void"
end
local emptyNode = {tag = "void"}

--TODO make a functor generating nodeNodes functions
local function nodeNum (num)
    return {tag = "number", val = tonumber(num)}
end
local function nodeVar (var)
    return {tag = "variable", var = var}
end
local function nodeAssign(id, exp)
    return {tag = "assign", id = id, exp = exp}
end
local function nodeSeq(st1, st2)
    return isNodeEmpty(st2) and st1 or {tag = "seq", st1 = st1, st2 = st2}
end
local function nodeRet(exp)
    return {tag = "return", exp = exp}
end
local function nodePrint(exp)
    return {tag = "print", exp = exp}
end

local ws = lpeg.S' \t\n'    --we might need ws or ws^1 in some places
local ws_ = ws^0

local comma = lpeg.S'.'

local Assign_ = lpeg.P'=' * ws_

local digit = lpeg.R'09'
local digits = digit^0
local hexdigit = lpeg.R('09', 'af', 'AF')
local hexdigits = hexdigit^0
local function numeralCapture(digit, comma)
    return (digit^0 * (comma * digit^0)^-1) - ((comma+'')*-(digit+comma))
end
--I might want to have coding numeral stuck to variable identifiers or very special operators, so no spaces at the end.
local numeral = ('0' * lpeg.S'xX' * numeralCapture(hexdigit, comma) + numeralCapture(digit, comma) * (lpeg.S'eE' * digit^1)^-1) / nodeNum

local alpha = lpeg.R('az', 'AZ')
local alphanum = alpha+digit

--no spaces, potentially, things like field access need to be stuck to the ID
local ID = lpeg.C((alpha + '_') * (alphanum + '_')^0)
local var = ID / nodeVar

local OP_ = '(' * ws_
local CP_ = ')' * ws_
local OB_ = '{' * ws_
local CB_ = '}' * ws_
local SC_ = ';' * ws_

local ret_ = "return" * ws_
local printStat_ = '@' * ws_

local function foldBin(a, op, b)
    return {
        tag = "binop",
        op = op,
        e1 = a,
        e2 = b,
    }
end
local function fold2Bin(a, opb)
    if opb == nil then return a end
    opb.e1 = a
    opb.tag = "binop"
    return opb
end
local function foldSuffBin(op, b)
    return {tag = "binopSuffix", e2 = b, op = op}
end
local function infixOpCapture(opPatt, abovePattern)
    return lpeg.Cf(abovePattern * (opPatt * abovePattern / foldSuffBin)^0, fold2Bin) 
end
local function infixOpCaptureRightAssoc(opPatt, selfPattern, abovePattern)  --set self to above to have a non asociative binary op.
    --return lpeg.Cg(abovePattern, 'fst') * lpeg.Cg(lpeg.Cb('fst') * (opPatt * selfPattern / foldSuffBin) / fold2Bin, 'fst')^-1 * lpeg.Cb('fst') --overkill
    return abovePattern * (opPatt * selfPattern / foldSuffBin)^-1 / fold2Bin
end
local function fold1Unary(op, a)
    return a and {tag = "unaryop", op=op, e = a} or op
end
local function unaryOpCapture(opPatt, selfPattern, abovePattern)  --allows chaining. set self to above to disallow
    return abovePattern + opPatt * ws_ * abovePattern / fold1Unary + opPatt * ws^1 * selfPattern / fold1Unary --not allowing ++ , -- or +- , but allowing - -
end
local function foldCompChain(t)
    --a < b < c will ultimatelybe transformed into (a<b) and (b<c)
    if #t == 1 then return t[1] end
    local r = {
        tag = "varop",
        eStack = Stack{},
        clause = "conjonction",
    }
    for i = 2, #t, 2 do
        r.eStack:push(foldBin(t[i-1], t[i], t[i+1]))
        --Sizeable issue, in a < b < c, expression b is duplicated.
--TODO : something smarter and later to compute it only once. for later.
    end
    return r
end
local function infixCompChainCapture(opPatt, abovePattern)
    return lpeg.Ct(abovePattern * (opPatt * abovePattern)^0) / foldCompChain
end

-- a list of cnstruct useable to build expression, from highest to lowest priorityst+2)))
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
return parse