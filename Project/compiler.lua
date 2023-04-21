local pt = require "pt".pt
local lpeg = require"lpeg"

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

--TODO put every thing inside the compiler. Compile => __call, codeGen => field and child.
--------------------------------------------------------------------------------
--TODO (low prio) legible code for debug mode.


local varsn = Symbol() ;
local metaCompiler = Prototype:new()
local Compiler = metaCompiler:new
{
    code = Stack{},
--TODO BiTable object ?
    --A biderectional table of variable names and their numerical index in the memory
    vars = setmetatable({[varsn] = 1},{
        __index = function(self, key)
            self[key] = self[varsn]
            self[self[varsn] ] = key
            self[varsn] = self[varsn] + 1
            return self[key]
        end,
    }),
    --A bidirectional table of labels and their numerical position
    labels = {},
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
        return Compiler.addCode(self, ast, assert(rawget(self.vars, ast.var), "Variable used before definition:\t" .. ast.var), nil)
end
local _invalidAst = Cargs(2) / zoo.invalidAst

local switch = {}
Compiler.switch = switch
local _codeDispPatt = C"exp" + C"stat"
--(recursively) expands expression and statements into relevant code
local codeGen = Compiler:new{}
Compiler.codeGen = codeGen
function codeGen:disp(ast, field)
        local new_ast = ast[field]
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
    end  * Cargs(2),
    [lpeg.Switch.default] = _invalidAst,
}

switch.stat = lpeg.Switch{
    void = lpeg.P'',
    ["if"] = Cargs(2) * Cc'exp_cond' / codeGen.disp * Cc"Zjmp" / Compiler.addCode,
    ["return"] = Cargs(2) * Cc'exp' / codeGen.disp * Cc'ret' / Compiler.addCode,
    print = Cargs(2) * Cc'exp' / codeGen.disp * Cc"print" / Compiler.addCode,
    assign = Cargs(2) * Cc'exp' / codeGen.disp * Cc'store' / Compiler.addCode / function(state, ast)
        state.code:push(state.vars[ast.id]) end  * Cargs(2),
    seq = Cargs(2) * Cc'stat1' / codeGen.disp * Cc'stat2' / codeGen.disp,
    [lpeg.Switch.default] = _invalidAst,
}

function metaCompiler:__call(ast)
    self.codeGen:stat(ast)
    self.code:push("push")
    self.code:push(0)
    self.code:push("ret")
--TODO add missing labels
    return self.code
end
--------------------------------------------------------------------------------
return Compiler