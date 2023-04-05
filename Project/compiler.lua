local pt = require "pt".pt
local utils = require "utils"

--------------------------------------------------------------------------------
--TODO delete this and use code : push directly. Or not, could be useful to generate legible opCode (can still be handled by code:push)
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

--TODO split codeExp into itself and codeStat. or not

local function codeExp(state, ast)
    if ast.tag == "void" then
    elseif ast.tag == "return" then
        codeExp(state, ast.exp)
        addCode(state, "ret")
    elseif ast.tag == "print" then
        codeExp(state, ast.exp)
        addCode(state, "print")
    elseif ast.tag == "number" then
        addCode(state, "push")
        addCode(state, ast.val)
    elseif ast.tag == "variable" then
        addCode(state, "load")
        --it's ok, addCode ignores the error msg passed as a second arguement
        addCode(state, assert(rawget(state.vars, ast.var), "Variable used before definition"))
    elseif ast.tag == "assign" then
        codeExp(state, ast.exp)
        addCode(state, "store")
        addCode(state, state.vars[ast.id])
    elseif ast.tag == "seq" then
        codeExp(state, ast.st1)
        codeExp(state, ast.st2)
    elseif ast.tag == "unaryop" then
        codeExp(state, ast.exp)
        addCode(state, unaryops[ast.op])
    elseif ast.tag == "binop" then
        codeExp(state, ast.e1)
        codeExp(state, ast.e2)
        addCode(state, binops[ast.op])
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
    else error("invalid ast : " .. pt(ast))
    end
end

local function compile (ast)
    local varsn = {} ; local state = {
        code = Stack{},
        vars = setmetatable({[varsn] = 1},{
            __index = function(self, key)
                self[key] = self[varsn]
                self[varsn] = self[varsn] + 1
                return self[key]
                
            end
        })
    }
    codeExp(state, ast)
    addCode(state, "push")
    addCode(state, 0)
    addCode(state, "ret")
    return state.code
end
--------------------------------------------------------------------------------
return compile