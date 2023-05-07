---@TODO : rewrite as a register machine in C++, Kotlin or Rust. Main factor for choice : memory allocation : Java : already has a GC.
---the compiler doesn't need to be writtent in Rust, but I will need to be rewritten, as the instructions set will change upon transitionning to a register machine.
---@TODO use enums to help lua-language-server helping me being consistent accross parser, compiler and interpreter

local pt = require "pt".pt
local lpeg = require"lpeg"

local Object = require"Object"
local Stack = require"Stack"
local Symbol = require"Symbol"

--local utils =
 require "utils"
local Promise = require "Promise"

--local parse = require "parser"

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
---@TODO Compiler.new

--[[
    Adds a single literal line to the code. Returns `self, ast`.
]]
function Compiler:addCode(opCode)
    self.code:push(opCode)
end
---Adds a single literal line to the code. Returns `self, ast`, for ease of chaining in the switch.
---@see Compiler.addCode
local function addCode(compiler, ast, opCode)
    compiler:addCode(opCode)
    return compiler, ast
end
--[[
    Adds a field verbatim to the code. Returns `self, ast`.
]]
function Compiler:addCodeField(ast, field)
    self:addCode(ast[field])
    return self, ast    --to allow chaining in pattern code blocks
end
---Adds a field verbatim to the code. Returns `self, ast`, for ease of chaining in the switch.
---@see Compiler.addCodeField
local function addCodeField(compiler, ast, field)
    return compiler:addCodeField(ast, field)
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
---@see Compiler.codeOP
local codeOP = setmetatable({}, {__index = function (self, k)
    self[k] = function (compiler, ast, op)
        return compiler, ast, compiler.codeOP[k][op or ast.op]
    end
    return rawget(self, k)
end})

---a florilege of functions use to handle varaibles
---@TODO why tf is it a compiler ??
---@TODO make that a field of the compiler, and create it upon new.
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

--[=[
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
--]=]

--[[
    A function which recursively generates the code of an AST
]]
function Compiler:codeGen(ast)
    self.switch(ast.tag, self, ast)
end
---A function which recursively generates the code of an AST
---@see Compiler.codeGen
local function codeGen(compiler, ast)
    return compiler:codeGen(ast)
end
--[[
    A function which expands the code of `ast[field]`. Returns `self, ast`.
]]
function Compiler:subCodeGen(ast, field)
    if ast[field] == nil then print(([[
        Warning empty field in subCodeGen while parsing %s, looking for field %s.
        It may be anything from a mistake in the parser or the compiler to someone's malpractice with empty statements.
        POSSIBLY A FORGOTTEN ':' BETWEEN codeGen and disp
        ]])
            :format(pt(ast), field))
        return self, ast
    end
    self:codeGen(ast[field])
    return self, ast
end
---A function which expands the code of `ast[field]`. Returns `self, ast`, for ease of chaining in the switch.
---@see Compiler.subCodeGen
local function subCodeGen(compiler, ast, field)
    return compiler:subCodeGen(ast, field)
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
    self:addCode(jmp)
    self:addCode(0)
    local pc = #self.code   --saving the current position
    p:zen(function (v)
        self.code[pc] = v - pc
    end)
    return p
end

---expands an assignement, depending on its left hand side's tag.
---@type fun(lhs_tag : string, state : Compiler, ast : table): Compiler, table
--[[@TODO see if you want to keep variables as a disinct lhs from indexed.<br>
Probably yes, because they can be loaded into a static region of memory and more easily accessed.<br>
Besides, allocation of custom objects may make things even worse.
]]
local switch_assign = lpeg.Switch{
    variable = Cargs(2) * Cc'exp' / subCodeGen * Cc'store' / addCode / function(self, ast)
        self:addCode(self.vars[ast.lhs.var]) end * Cargs(2),
    --[[
    I feel it's important to compile the lhs before the expression to assign.
    I'm pretty sure JS and C++ have different stances on it (don't remember which, and they did not always have one), which can been tested by putting border effects.
    The reasons behind my choice are as follows :

- It follows reading order.
- Depending on the lhs, I might want to treat the expression value differently (no big deal) or not at all.
For example ̀`[3,4,0,5,9]` may not mean the same physically whether it's a bool, short int, int, or double array, and as such, might not be translated the same in opCode.
    ]]
    indexed = Cargs(2) / function (self, ast)
        ---@TODO For much later : think about binding or trick like lua's obj:method, and how to make sort of currification, so that it's not just for the last field.
        self:subCodeGen(ast.lhs, 'exp_ref') ---@TODO thinkabout fields, arrays of arrays...
        self:subCodeGen(ast.lhs, 'exp_index')
        self:subCodeGen(ast, 'exp')
        return self:addCode(ast, 'set')
    end
}

---@param self Compiler
---@param ast AST
function Compiler:Xjunction(ast, jmp)
    self:codeGen(ast.exp1)
    local pEnd = self:jmp(jmp)
    self:addCode'pop'
    self:codeGen(ast.exp2)
    pEnd:honor(#self.code)
end
---@see Compiler.Xjunction
local function Xjunction (compiler, ast, jmp)
    return compiler:Xjunction(ast, jmp)
end

function Compiler:newArray (ast)
    if ast.exp_size then    --defined using size
        codeGen:disp(ast, 'exp_size')
        self:addCode'c_new'
        if ast.exp_default then
            --testing whether the size is an integer
            self:addCode'peek'
            self:addCode'push'
            self:addCode(1)
            self:addCode'mod'
            local pInt = self:jmp('jmpop_Z')
            self:addCode'error_array_size'
            --testing whether size >= 0
            pInt:honor(#self.code)
            self:addCode'peek'
            self:addCode'push'
            self:addCode(0)
            self:addCode'gt'
            local pEnd = self:jmp('jmpop_Z')

            --main loop
            local pLoop = Promise:honored(#self.code)
            codeGen:disp(ast, 'exp_default')
            self:addCode'c_set'
            self:addCode'push'
            self:addCode(1)
            self:addCode'sub'
            self:jmp('jmp_NZ', pLoop)
            pEnd:honor(#self.code)
        end
        self:addCode'pop'    --popping the index at the end of the loop, to leave the array at the top of the stack
    else    --defined with a literral
        self:addCode'push'
        self:addCode(#ast.values)
        self:addCode'new'
        for i = 1, #ast.values do
            self:addCode'push'
            self:addCode(i)
            codeGen:exp(ast.values[i])
            self:addCode'c_set'   --to keep the array
            self:addCode'pop'     --to get rid of the index
        end
    end
    return self, ast
end
---@see Compiler.newArray
local function newArray (compiler, ast)
    return compiler:newArray(ast)
end

--Using pattern matching to write code block of sort without having to explicitely write a new function per entry.
--Sometimes, writing a function is the shortest, especially when using and reuising arguments, but can still easily be incorporated in the pattern (cf variable)
--obfuscating ? 
---@TODO factorize Cargs(2) ? is it possible in any satisfactory way ?
---@TODO depreciate state_X
Compiler.switch = lpeg.Switch{
    number = Cargs(2) * Cc'write' / addCode * Cc'val' / addCodeField,
    variable = Cargs(2) * Cc"load" / addCode
        / zoo.getVariable,
       -- * ( ((Carg(1) * Cc"vars" / get) * (Carg(2) * Cc"var" / get) / rawget) * Cc"Variable used before definition" / assert / 1 ) / addCode,
    indexed = Cargs(2) * Cc'exp_ref' / subCodeGen * Cc'exp_index' / subCodeGen * Cc'get' / addCode,
    ---@TODO depending on how floats are treated, revisit.
    --I should be able to write it higher level. Either in the parser, but it seems more work, or here but it requires function accessing the stack
    new = Cargs(2) / newArray,
    Array = Cargs(2) * Cc'exp_size' / subCodeGen * Cc'new' / addCode, ---only returns a ref to the array, does not not assign it.
    unaryop = Cargs(2) * Cc'exp' / subCodeGen / codeOP.u / addCode,
    binop = Cargs(2) * Cc'exp1' / subCodeGen * Cc'exp2' / subCodeGen / codeOP.b / addCode,
    conjunction = Cc'jmp_Z' * Xjunction,
    disjunction = Cc'jmp_NZ' * Xjunction,
    compChain = Cargs(2) / function (self, ast)
        self:codeGen(ast.chain[1])
        local pFail = Promise:new()
        for i = 3, #ast.chain - 2, 2 do
            self:codeGen(ast.chain[i])
            self:addCode(Compiler.codeOP.c[ast.chain[i-1]])  --compares while consuming only the first argument
            self:jmp('jmpop_Z', pFail)
        end
        self:codeGen(ast.chain[#ast.chain])
        self:addCode(Compiler.codeOP.b[ast.chain[#ast.chain - 1]])  --compares while consuming both arguments
        if #ast.chain > 3 then
            local pEnd = self:jmp'jmp'
            pFail:honor(#self.code)
            --print("bla", ast.chain[#ast.chain - 1])
            self:addCode'pop'    --pop value
            self:addCode'push'
            self:addCode(0)
            pEnd:honor(#self.code)
        else    ---@TODO not letting a pending promise out in the open, not sure how necessary or sufficient it is. It probably isn't, to ponder when doing GC with promises.
            ---@TODO probably better, but catch not inplemented yet
            -- pFail.catch() pFail:betray()

            pFail:honor(nil)
        end
    end * Cargs(2),

    assign = Cargs(2) / function (self, ast)
        switch_assign(ast.lhs.tag, self, ast) --can't use `/ switch_assign` directly because `switch_assign` is actually a table.
    end,
--    [lpeg.Switch.default] = _invalidAst,
--}

---@TODO cleaning : don't use code.disp when not necessary
--switch.stat = lpeg.Switch{
    void = lpeg.P(true),
    ["if"] = Cargs(2) * Cc'exp_cond' / subCodeGen / function (self, ast)
        local pEndThen = self:jmp"jmpop_Z" --consuming the stack top value after a conditional jump.
        codeGen:disp(ast, "stat_then")

        if ast.stat_else then
            local pEndIf = self:jmp"jmp"
            pEndThen:honor(#self.code)    --it's ok, the interpreter adds 1
            codeGen:disp(ast, "stat_else")
            pEndIf:honor(#self.code)
        else
            pEndThen:honor(#self.code)
        end
    end * Cargs(2),
    ["while"] = Cargs(2) / function (self, ast)
        local pStart = Promise:honored(#self.code)
        codeGen:disp(ast, 'exp_cond')
        local pEnd = self:jmp"jmpop_Z"
        codeGen:disp(ast, 'stat')
        self:jmp("jmp", pStart)
        pEnd:honor(#self.code)
    end * Cargs(2),
    ["return"] = Cargs(2) * Cc'exp' / subCodeGen * Cc'ret' / addCode,
    print = Cargs(2) * Cc'exp' / subCodeGen * Cc'print' / addCode,
    stat_assign = Cargs(2) / function (self, ast)
        switch_assign(ast.lhs.tag, self, ast) --can't use `/ switch_assign` directly because `switch_assign` is actually a table.
    end,
    seq = Cargs(2) / function (self, ast) ---@param self Compiler
        for _, s in ipairs(ast.stats) do
            self:codeGen(s)
        end
    end * Cargs(2),
    [lpeg.Switch.default] = _invalidAst,
}

function metaCompiler:__call(ast) ---@param self Compiler
    self:addCode("write")
    self:addCode(0)
    self:codeGen(ast)
    self:addCode("ret") --elegant, now that everything is expression, but the empty program may not be valid any more
    return self.code
end
--------------------------------------------------------------------------------
return Compiler