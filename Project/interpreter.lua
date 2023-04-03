local lpeg = require "lpeg"
local pt = require "pt"

local Stack = {
    push = function(self, ...)
        for _,v in ipairs {...} do
            table.insert(self, v)
        end
    end,
    pop = table.remove,
    unpack = table.unpack,
    __call = function(self, n)
        return type(n) == "number" and (n > 0 and self[n] or self[#self + n])
    end,
}
Stack.__index = Stack
setmetatable(Stack, {
    __call = function(self, t) return setmetatable(t, self) end,
})

--------------------------------------------------------------------------------
local function nodeNum (num)
    return {tag = "number", val = tonumber(num)}
end

local ws = lpeg.S' \t\n'    --we might need ws or ws^1 in some places
local wss = ws^0
local comma = lpeg.S'.'
local digit = lpeg.R'09'
local digits = digit^0
local hexdigit = lpeg.R('09', 'af', 'AF')
local hexdigits = hexdigit^0
local function numeralCapture(digit, comma)
    return (digit^0 * (comma * digit^0)^-1) - ((comma+'')*-(digit+comma))
end
--local numeral = (('0' * lpeg.S'xX' * hexdigits * (comma * hexdigits)^-1) - numeralVoidPredicate +
--  digits * (comma * digits)^-1 - numeralVoidPredicate) / nodeNum
local numeral = ('0' * lpeg.S'xX' * numeralCapture(hexdigit, comma) + numeralCapture(digit, comma) * (lpeg.S'eE' * digit^1)^-1) / nodeNum

local OP = '(' * wss
local CP = ')' * wss

local opA = lpeg.C(lpeg.S'+-') * wss
local opM = lpeg.C(lpeg.S'*/%') * wss
local opP = lpeg.C(lpeg.S'^') * wss

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
    return abovePattern + opPatt * wss * abovePattern / fold1Unary + opPatt * ws^1 * selfPattern / fold1Unary --not allowing ++ , -- or +- , but allowing - -
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

local function exp(i)
    return lpeg.V("exp" .. (i or ''))
end

local expOpList = Stack{}   -- a list of cnstruct useable to build expression, from highest to lowest priorityst+2)))
expOpList:push(numeral * wss + OP * exp() * CP) --primary
expOpList:push(unaryOpCapture(lpeg.C(lpeg.S'+-'), exp(#expOpList + 1), exp(#expOpList))) --unary +-
expOpList:push(infixOpCaptureRightAssoc(lpeg.C(lpeg.S'^') * wss, exp(#expOpList+1),  exp(#expOpList))) --power
expOpList:push(infixOpCapture(lpeg.C(lpeg.S'*/%') * wss, exp(#expOpList))) --multiplication
expOpList:push(infixOpCapture(lpeg.C(lpeg.S'+-') * wss, exp(#expOpList))) --addition
expOpList:push(infixCompChainCapture(lpeg.C(lpeg.S'<>' * lpeg.P'='^-1 + lpeg.S'!=' * '=') * wss, exp(#expOpList))) --comparison
    
local expGrammar = {exp(),
}
for i = 1, #expOpList do
    expGrammar["exp"..i] = expOpList[i]
end
expGrammar["exp"] = exp(#expOpList)

expGrammar = wss * lpeg.P(expGrammar) * -1
local function parse (input)
    return expGrammar:match(input)
end

--------------------------------------------------------------------------------
local function addCode(state, opCode)
    local code = state.code
    code:push(opCode)
end

local binops = {
    ['+'] = "add",
    ['-'] = "sub",
    ['*'] = "mul",
    ['/'] = "div",
    ['%'] = "mod",
    ['^'] = "pow",

    ['<'] = "lt",
    ['>'] = "gt",
    ['>='] = "ge",
    ['<='] = "le",
    ['=='] = "eq",
    ['!='] = "neq",
}
local unaryops = {
    ['+'] = "plus",
    ['-'] = "minus",
}
local function codeExp(state, ast)
    if ast.tag == "number" then
        addCode(state, "push")
        addCode(state, ast.val)
    elseif ast.tag == "binop" then
        codeExp(state, ast.e1)
        codeExp(state, ast.e2)
        addCode(state, binops[ast.op])
    elseif ast.tag == "unaryop" then
        codeExp(state, ast.e)
        addCode(state, unaryops[ast.op])
    elseif ast.tag == "varop" then
        if ast.clause == "conjonction" then
            codeExp(state, ast.eStack[1])
            for i = 2, #ast.eStack do
                codeExp(state, ast.eStack[i])
                addCode(state, "mul")
            end
        else
            error("invalid varop, unknown clause : " .. ast.clause)
        end
    else error("invalid ast : " .. pt.pt(ast))
    end
end

local function compile (ast)
    local state = {code = Stack{}}
    codeExp(state, ast)
    return state.code
end

--------------------------------------------------------------------------------
local function run(code, stack)
    stack = stack or Stack{}
    local pc = 1
    local trace
    local function push( ...)
        trace:push('<- ' .. table.concat{...})
        stack:push(...)
    end
    local function pop()
        local value = stack:pop()
        trace:push('-> ' .. value)
        return value
    end
    while pc <= #code do
        trace = Stack{"instruction: " .. code[pc]}
        if code[pc] == "push" then
            pc = pc + 1
            push(code[pc])
        elseif code[pc] == "add" then
            local top = pop()
            push(pop() + top)
        elseif code[pc] == "sub" then
            local top = pop()
            push(pop() - top)
        elseif code[pc] == "mul" then
            local top = pop()
            push(pop() * top)
        elseif code[pc] == "div" then
            local top = pop()
            push(pop() / top)
        elseif code[pc] == "mod" then
            local top = pop()
            push(pop() % top)
        elseif code[pc] == "pow" then
            local top = pop()
            push(pop() ^ top)
        elseif code[pc] == "plus" then
        elseif code[pc] == "minus" then
            push(-pop())

        elseif code[pc] == "lt" then
            local top = pop()
            push(pop() < top and 1 or 0)
        elseif code[pc] == "gt" then
            local top = pop()
            push(pop() > top and 1 or 0)
        elseif code[pc] == "ge" then
            local top = pop()
            push(pop() >= top and 1 or 0)
        elseif code[pc] == "le" then
            local top = pop()
            push(pop() <= top and 1 or 0)
        elseif code[pc] == "eq" then
            local top = pop()
            push(pop() == top and 1 or 0)
        elseif code[pc] == "neq" then
            local top = pop()
            push(pop() ~= top and 1 or 0)
        else
            print(trace:unpack())
            error("unknown instruction")-- : " .. code[pc])
        end
        print(trace:unpack())
        pc = pc + 1
    end
    return stack[1]
end

local input = io.read()
print(input)
local ast = parse (input)
print(pt.pt(ast))
print()
local code = compile((ast))
print(pt.pt(code))
print()
print(run(code))
--]]