local pt = require "pt".pt
local lpeg = require"lpeg"

local utils = require "utils"

--------------------------------------------------------------------------------
--TODO (low prio) legible code for debug mode.

local addCode = function(state, ast, opCode)
    state.code:push(opCode)
    return state, ast    --to allow chaining in pattern code blocks
end
local addCodeField = function(state, ast, field)
    state.code:push(ast[field])
    return state, ast    --to allow chaining in pattern code blocks
end

local codeOP = {
    u = {
        ['+'] = "plus",
        ['-'] = "minus",
        ['~'] = "not",
    },
    b = {
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
    },
}

local invalidAst = Cargs(2) / function (state, ast) error("invalid ast : " .. pt(ast), 2) end

local switch = {}
local _Gmeta = utils.set_GlpegShortHands"C"
local _codeDispPatt = C"exp" + C"stat"
--(recursively) expands expression and statements into relevant code
local codeGen = {
    disp = function(state, ast, field)
            local new_ast = ast[field]
            switch[_codeDispPatt:match(field)](new_ast.tag, state, new_ast)
        return state, ast
    end,
    exp = function(state, ast)
        switch.exp(ast.tag, state, ast)
    end,
    stat = function(state, ast)
        switch.stat(ast.tag, state, ast)
    end,
}

utils.set_GlpegShortHands"Cc"
--Using pattern matching to write code block of sort without having to explicitely write a new function per entry.
--Sometimes, writing a function is the shortest, especially when using and reuising arguments, but can still easily be incorporated in the pattern (cf variable)
--obfuscating ? 
--TODO factorize Cargs(2) ? is it possible in any satisfactory way ?
switch.exp = lpeg.Switch{
    number = Cargs(2) * push / addCode * val / addCodeField,
    variable = Cargs(2)* Cc"load" / addCode ---[=[ 
        / function(state, ast)
    -- BEWARE : assert returns all of its args upon success. Here, the function addCode takes care of ignoring the error msg. Otherwise, assert(...), nil would be useful
       return addCode(state, ast, assert(rawget(state.vars, ast.var), "Variable used before definition:\t" .. ast.var), nil) end,
    --]=]
       -- * ( ((Carg(1) * Cc"vars" / get) * (Carg(2) * Cc"var" / get) / rawget) * Cc"Variable used before definition" / assert / 1 ) / addCode,
    unaryop = Cargs(2) * exp / codeGen.disp * (Carg(2) * op / get / codeOP.u) / addCode,
    binop = Cargs(2) * exp1 / codeGen.disp * exp2 / codeGen.disp * (Carg(2) * op / get / codeOP.b) / addCode,
    --Could substitution be used for branching ?
    varop = Cargs(2) / function (state, ast)
        if ast.clause == "conjonction" then
            codeGen.exp(state, ast.eStack[1])
            for i = 2, #ast.eStack do
                codeGen.exp(state, ast.eStack[i])
                state.code:push"mul"
            end
        else
            error("invalid varop, unknown clause : " .. ast.clause)
        end
    end  * Cargs(2),
    [lpeg.Switch.default] = invalidAst,
}

switch.stat = lpeg.Switch{
    void = lpeg.P'',
    ["return"] = Cargs(2) * exp / codeGen.disp * ret / addCode,
    print = Cargs(2) * exp / codeGen.disp * Cc"print" / addCode,
    assign = Cargs(2) * exp / codeGen.disp * store / addCode / function(state, ast)
        state.code:push(state.vars[ast.id]) end  * Cargs(2),
    seq = Cargs(2) * stat1 / codeGen.disp * stat2 / codeGen.disp,
    [lpeg.Switch.default] = invalidAst,
}

local function compile (ast)
    local varsn = {} ; local state = {
        code = Stack{},
        vars = setmetatable({[varsn] = 1},{
            __index = function(self, key)
                self[key] = self[varsn]
                self[self[varsn] ] = key
                self[varsn] = self[varsn] + 1
                return self[key]
            end,
        })
    }
    --codeStat(state, ast)
    --switch.stat(state, ast)
    codeGen.stat(state, ast)
    state.code:push("push")
    state.code:push(0)
    state.code:push("ret")
    return state.code
end

--------------------------------------------------------------------------------
setmetatable(_G, _Gmeta)
return compile