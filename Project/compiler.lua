---@TODO : rewrite as a register machine in C++, Kotlin or Rust. Main factor for choice : memory allocation : Java : already has a GC.
---the compiler doesn't need to be writtent in Rust, but I will need to be rewritten, as the instructions set will change upon transitionning to a register machine.
---@TODO use enums to help lua-language-server helping me being consistent accross parser, compiler and interpreter

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
---@TODO (low prio) legible code for debug mode.

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

---@TODO raise error when incorrect operator is used.
Compiler.codeOP = {
    u = {
        ['+'] = 'plus',
        ['-'] = 'minus',
        ['!'] = 'not',
    },
    b = {
        ['+'] = 'add',
        ['-'] = 'sub',
        ['*'] = 'mul',
        ['/'] = 'div',
        ['%'] = 'mod',
        ['^'] = 'pow',

        ['&&'] = 'and',
        ['||'] = 'or',
    },
    c = {
        ['<'] = 'c_lt',
        ['>'] = 'c_gt',
        ['>='] = 'c_ge',
        ['<='] = 'c_le',
        ['=='] = 'c_eq',
        ['!='] = 'c_neq',
    }
}
for k, v in pairs(Compiler.codeOP.c) do
    Compiler.codeOP.b[k] = v:sub(3)
end

---a florilege of useful functions
---@TODO why tf is it a compiler ??
local zoo = Compiler:new()
function zoo:invalidAst(ast)
    error("invalid ast : " .. pt(ast), 2)
end
function zoo:getVariable(ast)
    -- BEWARE : assert returns all of its args upon success. Here, the function addCode takes care of ignoring the error msg. Otherwise, assert(...), nil would be useful
---@diagnostic disable-next-line: redundant-parameter
        return Compiler.addCode(self, ast, assert(rawget(self.vars, ast.var), ("Variable used before definition:\t%s at opCode line:\t%s.\nAST:\t%s"):format(ast.var, #self.code, pt(ast))), nil)
end
local _invalidAst = Cargs(2) / zoo.invalidAst

local switch = {}
--relevant for metaCompiler.__call I guess
Compiler.switch = switch
local _codeDispPatt = C'exp' + C'stat'
--(recursively) expands expression and statements into relevant code
local codeGen = Compiler:new{}
--relevant for metaCompiler.__call I guess
Compiler.codeGen = codeGen
function codeGen:disp(ast, field)
    if ast[field] == nil then print(([[
Warning empty field in codeGen.disp while parsing %s, looking for field %s.
It may be anything from a mistake in the parser or the compiler to user malpractice with empty statements.
POSSIBLY A FORGOTTEN ':' BETWEEN codeGen and disp
]])
    :format(pt(ast), field)) return self, ast end
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

--[[
function writing a goto to a position described by the next line of code.
***
You don't need to know the position of the target label in advance.
Returns a Promise to be honored to that label position.
***
Promises are a bit overkill here, but useful technique later on to implement other control structures, in particular gotos.
Besides, it makes it so I can use nearly the same syntax reguardless of whether the label is already figured out, and how many gotos go to the same label

The real reason for me to use Promises though, is, as you may guess, for the pleasure of implementing them.
]]
---@return Promise
function Compiler:jmp(jmp, p)
    p = p or Promise:new()
    self.code:push(jmp)
    self.code:push(0)
    local pc = #self.code   --saving the current position
    p:zen(function (v)
        self.code[pc] = v - pc
    end)
    return p
end

---expands an assignement, depending on its left hand side's tag.
---@type fun(lhs_tag : string, state : Compiler, ast : table): Compiler, table
switch._assign = lpeg.Switch{
    ---@TODO
    ---see if you want to keep variables as a disinct lhs from indexed.<br>
    ---Probably yes, because they can be loaded into a static region of memory and more easily accessed.<br>
    ---Besides, allocation of custom objects may make things even worse.
    variable = Cargs(2) * Cc'exp' / codeGen.disp * Cc'store' / Compiler.addCode / function(state, ast)
        state.code:push(state.vars[ast.lhs.var]) end * Cargs(2),
    --[[
    I feel it's important to compile the lhs before the expression to assign.
    I'm pretty sure JS and C++ have different stances on it (don't remember which, and they did not always have one), which can been tested by putting border effects.
    The reasons behind my choice are as follows :

- It follows reading order.
- Depending on the lhs, I might want to treat the expression value differently (no big deal) or not at all.
For example ̀`[3,4,0,5,9]` may not mean the same physically whether it's a bool, short int, int, or double array, and as such, might not be translated the same in opCode.
    ]]
    indexed = Cargs(2) / function (state, ast)
        ---@TODO For much later : think about binding or trick like lua's obj:method, and how to make sort of currification, so that it's not just for the last field.
        codeGen:disp(ast.lhs, 'exp_ref') ---@TODO thinkabout fields, arrays of arrays...
        codeGen:disp(ast.lhs, 'exp_index')
        codeGen:disp(ast, 'exp')
        return state:addCode(ast, 'set')
    end
}

function Compiler:Xjunction(jmp)
    return Cargs(2) / function (state, ast)
        codeGen.exp(state, ast.exp1)
        local pEnd = state:jmp(jmp)
        state.code:push'pop'
        codeGen.exp(state, ast.exp2)
        pEnd:honor(#state.code)
    end * Cargs(2)
end

--Using pattern matching to write code block of sort without having to explicitely write a new function per entry.
--Sometimes, writing a function is the shortest, especially when using and reuising arguments, but can still easily be incorporated in the pattern (cf variable)
--obfuscating ? 
---@TODO factorize Cargs(2) ? is it possible in any satisfactory way ?
switch.exp = lpeg.Switch{
    number = Cargs(2) * Cc'push' / Compiler.addCode * Cc'val' / Compiler.addCodeField,
    variable = Cargs(2) * Cc"load" / Compiler.addCode
        / zoo.getVariable,
       -- * ( ((Carg(1) * Cc"vars" / get) * (Carg(2) * Cc"var" / get) / rawget) * Cc"Variable used before definition" / assert / 1 ) / addCode,
    indexed = Cargs(2) * Cc'exp_ref' / codeGen.disp * Cc'exp_index' / codeGen.disp * Cc'get' / Compiler.addCode,
    new = Cargs(2) / function (state, ast)
        codeGen:disp(ast, 'exp_size')
        state.code:push'c_new'
        if ast.exp_default then
            local pStart = Promise:honored(#state.code)
            codeGen:disp(ast, 'exp_default')
            state.code:push'c_set'
            state.code:push'push'
            state.code:push(1)
            state.code:push'sub'
            state:jmp('jmp_NZ', pStart)
            state.code:push'pop'
        elseif ast.init then
            state.code:push'init'
            table.move(ast.init, 1, #ast.init, #state.code, state.code)
        else
            state.code:push'pop'
        end
    end,
    Array = Cargs(2) * Cc'exp_size' / codeGen.disp * Cc'new' / Compiler.addCode, ---only returns a ref to the array, does not not assign it.
    unaryop = Cargs(2) * Cc'exp' / codeGen.disp * (Carg(2) * Cc'op' / get / Compiler.codeOP.u) / Compiler.addCode,
    binop = Cargs(2) * Cc'exp1' / codeGen.disp * Cc'exp2' / codeGen.disp * (Carg(2) * Cc'op' / get / Compiler.codeOP.b) / Compiler.addCode,
    conjunction = Compiler:Xjunction'jmp_Z',
    disjunction = Compiler:Xjunction'jmp_NZ',
    compChain = Cargs(2) / function (state, ast)
        codeGen.exp(state, ast.chain[1])
        local pFail = Promise:new()
        for i = 3, #ast.chain - 2, 2 do
            codeGen.exp(state, ast.chain[i])
            state.code:push(Compiler.codeOP.c[ast.chain[i-1]])  --compares while consuming only the first argument
            state:jmp('jmpop_Z', pFail)
        end
        codeGen.exp(state, ast.chain[#ast.chain])
        state.code:push(Compiler.codeOP.b[ast.chain[#ast.chain - 1]])  --compares while consuming both arguments
        if #ast.chain > 3 then
            local pEnd = state:jmp'jmp'
            pFail:honor(#state.code)
            --print("bla", ast.chain[#ast.chain - 1])
            state.code:push'pop'    --pop value
            state.code:push'push'
            state.code:push(0)
            pEnd:honor(#state.code)
        else    ---@TODO not letting a pending promise out in the open, not sure how necessary or sufficient it is. It probably isn't, to ponder when doing GC with promises.
            ---@TODO probably better, but catch not inplemented yet
            -- pFail.catch() pFail:betray()

            pFail:honor(nil)
        end
    end * Cargs(2),
    [lpeg.Switch.default] = _invalidAst,
}

---@TODO cleaning : don't use code.disp when not necessary, use single quote instead of double.
switch.stat = lpeg.Switch{
    void = lpeg.P'',
    ["if"] = Cargs(2) * Cc'exp_cond' / codeGen.disp / function (state, ast)
        local pEndThen = state:jmp"jmpop_Z" --consuming the stack top value after a conditional jump.
        codeGen:disp(ast, "stat_then")

        if ast.stat_else then
            local pEndIf = state:jmp"jmp"
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
        local pEnd = state:jmp"jmpop_Z"
        codeGen:disp(ast, 'stat')
        state:jmp("jmp", pStart)
        pEnd:honor(#state.code)
    end * Cargs(2),
    ["return"] = Cargs(2) * Cc'exp' / codeGen.disp * Cc'ret' / Compiler.addCode,
    print = Cargs(2) * Cc'exp' / codeGen.disp * Cc"print" / Compiler.addCode,
    assign = Cargs(2) / function (state, ast)
        switch._assign(ast.lhs.tag, state, ast) --can't use `/ switch._assign` directly because `switch._assign` is actually a table.
    end,
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