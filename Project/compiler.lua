local pt = require "pt".pt
local lpeg = require"lpeg"

local utils = require "utils"

local _Gmeta = utils.set_GlpegShortHands"Cc"

--------------------------------------------------------------------------------


--TODO delete this and use code : push directly.
--TODO (low prio) legible code for debug mode
--local function state.code:push(opCode)
--    state.code:push(opCode)
--end

--TODO (low prio) make enums and make opCode truly only numbers.
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

--TODO custom switch-like
--TODO treat hard cases first, easy ones in an else switch
--{
--    number = {"push", "ast.val"}
--  unaryop = {ast.exp, ast.}   --recognizes which addCode fns to use based on name
--}

local addCode = function(state, ast, field)
    if field ~= nil then state.code:push(ast[field])
    else state.code:push(ast) end
end

--[[
local codeDisp = {
    bop = lpeg.P'',
    uop = lpeg.P'',
    vop = lpeg.P'',
}
for _, e in pairs{
    "exp", "stat"
} do
    codeDisp[e] = lpeg.Carg(1) * lpeg.Carg(2) * lpeg.P(e) / codeBit
end
--]]

local Carg = lpeg.Carg
local Cargs = setmetatable ({
},{
    __call = function(self, i, j)
        if j == nil then
            return self[1][i]
        else
            return self[i][j]
--            return lpeg.Cc(i) * self[j] / select      --neat alternative to only store 1 -> i ranges
        end
    end,
    __index = function(self, i)
        self[i] = setmetatable({}, {
            __index = function(j)
                if j < i then
                    return lpeg.P''
                else
                    return self[i][j-1] * Carg(j)
                end
            end
        })
        return self[i]
    end,
})

--[[
local switchExp = Carg(1) * (
--TODO I dont think you have any guaranty on order of excution, it might be an idea to use lpeg.Cmt (once the tag is matched, is ok, but its ugly).
      "number" * lpeg.Cc"push" / addCode * Cargs(2) * lpeg.Cc"val" / codeDisp
    + "variable" * lpeg.Cc"load" / 
    + "unaryop" * lpeg.Cc""
    + "binop" * lpeg.Cc""
    + "varop" * lpeg.Cc""
)
]]
local function codeExp(state, ast)
    if ast.tag == "number" then
        state.code:push("push")
        state.code:push(ast.val)
    elseif ast.tag == "variable" then
        state.code:push("load")
        -- `,nil` since assert returns all his args upon success.
        state.code:push(assert(rawget(state.vars, ast.var), "Variable used before definition"), nil)
    elseif ast.tag == "unaryop" then
        codeExp(state, ast.exp)
        state.code:push(unaryops[ast.op])
    elseif ast.tag == "binop" then
        codeExp(state, ast.exp1)
        codeExp(state, ast.exp2)
        state.code:push(binops[ast.op])
    elseif ast.tag == "varop" then
        if ast.clause == "conjonction" then
            codeExp(state, ast.eStack[1])
            for i = 2, #ast.eStack do
                codeExp(state, ast.eStack[i])
                state.code:push("mul")
            end
        else
            error("invalid varop, unknown clause : " .. ast.clause)
        end
    else error("invalid ast : " .. pt(ast))
    end
end

local function codeStat(state, ast)
    if ast.tag == "void" then
    elseif ast.tag == "return" then
        codeExp(state, ast.exp)
        state.code:push("ret")
    elseif ast.tag == "print" then
        codeExp(state, ast.exp)
        state.code:push("print")
    elseif ast.tag == "assign" then
        codeExp(state, ast.exp)
        state.code:push("store")
        state.code:push(state.vars[ast.id])
    elseif ast.tag == "seq" then
        codeStat(state, ast.stat1)
        codeStat(state, ast.stat2)
    else error("invalid ast : " .. pt(ast))
    end
end

local function compile (ast)
    local varsn = {} ; local state = {
        code = Stack{},
        vars = setmetatable({[varsn] = 1},{
            __index = function(self, key)
                self[key] = self[varsn]
                self[self[varsn]] = key
                self[varsn] = self[varsn] + 1
                return self[key]
            end,
        })
    }
    codeStat(state, ast)
    state.code:push("push")
    state.code:push(0)
    state.code:push("ret")
    return state.code
end
--------------------------------------------------------------------------------
setmetatable(_G, _Gmeta)
return compile