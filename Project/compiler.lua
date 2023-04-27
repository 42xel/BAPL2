local pt = require "pt".pt
local lpeg = require"lpeg"

local Object = require"Object"
local Stack = require"Stack"
local Symbol = require"Symbol"

local utils = require "utils"
local Promise = require "Promise"

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

--------------------------------------------------------------------------------
--TODO (low prio) legible code for debug mode.

local varsn = Symbol() ;
local metaCompiler = Object:new()
---@class Compiler : Object
---@field code Stack
---@field vars table
---@field new fun (self:Compiler, t?:table) : Compiler
local Compiler = metaCompiler:new
{
    code = Stack{},
    --A biderectional table of variable names and their numerical index in the memory
    vars = setmetatable({[varsn] = 1},{
        __index = function(self, key)
            self[key] = self[varsn]
            self[self[varsn] ] = key
            self[varsn] = self[varsn] + 1
            return self[key]
        end,
    }),
--    --A  table of labels and their numerical position
--    labels = setmetatable({},{
--        __index = function (self, key)
--            self[key] = Promise:new()
--            return self[key]
--        end,
--    })
}

function Compiler:addCode(ast, opCode)
    self.code:push(opCode)
    return self, ast    --to allow chaining in pattern code blocks
end
function Compiler:addCodeField(ast, field)
    self.code:push(ast[field])
    return self, ast    --to allow chaining in pattern code blocks
end

--TODO raise error when incorrect operator is used.
Compiler.codeOP = {
    u = {
        ['+'] = "plus",
        ['-'] = "minus",
        ['!'] = "not",
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

--a florilege of useful functions
local zoo = Compiler:new()
function zoo:invalidAst(ast)
    error("invalid ast : " .. pt(ast), 2)
end
function zoo:getVariable(ast)
    -- BEWARE : assert returns all of its args upon success. Here, the function addCode takes care of ignoring the error msg. Otherwise, assert(...), nil would be useful
---@diagnostic disable-next-line: redundant-parameter
        return Compiler.addCode(self, ast, assert(rawget(self.vars, ast.var), "Variable used before definition:\t" .. ast.var), nil)
end
local _invalidAst = Cargs(2) / zoo.invalidAst

local switch = {}
--relevant for metaCompiler.__call I guess
Compiler.switch = switch
local _codeDispPatt = C"exp" + C"stat"
--(recursively) expands expression and statements into relevant code
local codeGen = Compiler:new{}
--relevant for metaCompiler.__call I guess
Compiler.codeGen = codeGen
function codeGen:disp(ast, field)
    if ast[field] == nil then print(([[
Warning empty field in codeGen.disp while parsing %s, looking for field %s.
It may be anything from a mistake in the parser or the compiler to user malpractice with empty statements.
THAT OR AN EFFING FORGOTTEN ':' BETWEEN codeGen and disp]])
    :format(pt(ast), field)) return self, ast end
    print(pt(ast))
    local new_ast = ast[field]
    print(pt(new_ast))
    self.switch[_codeDispPatt:match(field)](new_ast.tag, self, new_ast)
    return self, ast
end
function codeGen:exp(ast)
    self.switch.exp(ast.tag, self, ast)
end
function codeGen:stat(ast)
    self.switch.stat(ast.tag, self, ast)
end

--Using pattern matching to write code block of sort without having to explicitely write a new function per entry.
--Sometimes, writing a function is the shortest, especially when using and reuising arguments, but can still easily be incorporated in the pattern (cf variable)
--obfuscating ? 
--TODO factorize Cargs(2) ? is it possible in any satisfactory way ?
switch.exp = lpeg.Switch{
    number = Cargs(2) * Cc'push' / Compiler.addCode * Cc'val' / Compiler.addCodeField,
    variable = Cargs(2)* Cc"load" / Compiler.addCode
        / zoo.getVariable,
       -- * ( ((Carg(1) * Cc"vars" / get) * (Carg(2) * Cc"var" / get) / rawget) * Cc"Variable used before definition" / assert / 1 ) / addCode,
    unaryop = Cargs(2) * Cc'exp' / codeGen.disp * (Carg(2) * Cc'op' / get / Compiler.codeOP.u) / Compiler.addCode,
    binop = Cargs(2) * Cc'exp1' / codeGen.disp * Cc'exp2' / codeGen.disp * (Carg(2) * Cc'op' / get / Compiler.codeOP.b) / Compiler.addCode,
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
    end * Cargs(2),
    [lpeg.Switch.default] = _invalidAst,
}

--[[
function writing a goto
***
You don't need to know the position of the target label in advance.
Returns a Promise to be honored to that label position.
***
it would be simpler to only put the jump index in the opCode, but I get the feeling that pushing it on the stack opens the door to function pointers
***
Promises are a bit overkill here, but useful technique later on to implement other control structures, in particular gotos.
Besides, it makes it so I can use nearly the same syntax reguardless of whether the label is already figured out, and how many gotos go to the same label

The real reason for me to use Promises though, is, as you may guess, for the pleasure of implementing them.
]]
---@return Promise
function Compiler:labelPromise(jmp, p)
    p = p or Promise:new()
    self.code:push"push"
    self.code:push(0)
    local pc = #self.code   --saving the current position
    p:zen(function (v)
        self.code[pc] = v - (pc + 1)  --pc is the position of the jump value, pc+1 is the position of the "Zjmp" instruction.
    end)
    self.code:push(jmp)
    return p
end

---@TODO cleaning : don't use code.disp when not necessary, use single quote instead of double.
switch.stat = lpeg.Switch{
    void = lpeg.P'',
    ["if"] = Cargs(2) * Cc'exp_cond' / codeGen.disp / function (state, ast)
        local pEndThen = state:labelPromise"Zjmp"
        codeGen:disp(ast, "stat_then")

        if ast.stat_else then
            local pEndIf = state:labelPromise"jmp"
            pEndThen:honor(#state.code)    --it's ok, the interpreter adds 1
            codeGen:disp(ast, "stat_else")
            pEndIf:honor(#state.code)
        else
            pEndThen:honor(#state.code)
        end
    end * Cargs(2),
    ["while"] = Cargs(2) / function (state, ast)
        local pStart = Promise:honored(#state.code)
        codeGen:disp(ast, 'exp_cond')
        local pEnd = state:labelPromise"Zjmp"
        codeGen:disp(ast, 'stat')
        state:labelPromise("jmp", pStart)
        pEnd:honor(#state.code)
    end * Cargs(2),
    ["return"] = Cargs(2) * Cc'exp' / codeGen.disp * Cc'ret' / Compiler.addCode,
    print = Cargs(2) * Cc'exp' / codeGen.disp * Cc"print" / Compiler.addCode,
    assign = Cargs(2) * Cc'exp' / codeGen.disp * Cc'store' / Compiler.addCode / function(state, ast)
        state.code:push(state.vars[ast.id]) end * Cargs(2),
        --state.code:push(state.vars[ast.id]) end * Cargs(2),
    seq = Cargs(2) / function (state, ast) ---@param state Compiler
        for _, s in ipairs(ast.stats) do
            state.codeGen:stat(s)
        end
    end * Cargs(2),
    [lpeg.Switch.default] = _invalidAst,
}

function metaCompiler:__call(ast) ---@param self Compiler
    self.codeGen:stat(ast)
    self.code:push("push")
    self.code:push(0)
    self.code:push("ret")
    return self.code
end
--------------------------------------------------------------------------------
return Compiler