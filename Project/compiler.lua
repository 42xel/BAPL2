---@TODO : rewrite as a register machine in C++, Kotlin or Rust. Main factor for choice : memory allocation : Java : already has a GC.
---the compiler doesn't need to be writtent in Rust, but I will need to be rewritten, as the instructions set will change upon transitionning to a register machine.
---@TODO use enums to help lua-language-server helping me being consistent accross parser, compiler and interpreter
---@TODO use auxiliary files for auxiliary functions ? Like one file one small responsibility ?
---@type boolean
if _COMPILER_DEBUG == nil then
    _COMPILER_DEBUG = true
end

local pt = require "pt".pt
local lpeg = require"lpeg"

--local Object = require"Object"
local Stack = require"Stack"
local Symbol = require"Symbol"

local Node = require"ASTNode"
local nodeAssign = Node.nodeAssign

--local utils =
 require "utils"
local Promise = require "Promise"
--local Proxy = require "Proxy"

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

--------------------------------------------------------------------------------
---@TODO (low prio) legible code for debug mode.

---@class Compiler : table
---@field code Stack
---@field vars table A table of variable names and their absolute numerical index for reference.
---@field ctx table A table of variable names and their numerical index in the local memory, as well as correspondance to their absolute global reference.
---@field switch lpegSwitch The main switch. Recursively compiles the ast, depending on their tag, and returns the current compiler and AST for ease of chaining
----@field zoo table a florilege of functions use to handle varaibles
---@field new fun (self:Compiler, t?:table) : Compiler
local Compiler = {}

--[[
    Adds a single literal line to the code.
]]
function Compiler:addCode(opCode)
    self.code:push(opCode)
end
--[[
    Adds a single literal line to the code. Returns `self, ast`.
]]
function Compiler:c_addCode(ast,opCode)
    self.code:push(opCode)
    return self, ast
end
--[[
    Adds a field verbatim to the code. Returns `self, ast`.
]]
function Compiler:addCodeField(ast, field)
    self:addCode(ast[field])
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
        ['//'] = 'idiv',
        ['%'] = 'mod',
        ['^'] = 'pow',

        ['&&'] = 'and',
        ['||'] = 'or',
        ['=>'] = 'imply',
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

function Compiler:invalidAst(ast)
    error("invalid ast : " .. pt(ast), 2)
    return self, ast
end
---@TODO maybe remove the warning, because of functions ?
function Compiler:getVariable(ast, vars)
    local ctx = self.ctx
    --print("Compiler:getVariable() 0:\t", ast.var, ast.prefix, pt(ctx), pt(vars))
    if not ast.prefix then
        ast.prefix = 0
        --print("Compiler:getVariable() 1:\t", ast.var, ast.prefix, pt(ctx))
    end
    for _ = 1, ast.prefix do
        ctx = ctx.parent
        --print("Compiler:getVariable() 2:\t", ast.var, ast.prefix, pt(ctx))
    end
    while not rawget(ctx, ast.var) do
        ast.prefix = ast.prefix + 1
        ctx = rawget(ctx, 'parent')
        --print("Compiler:getVariable() 3:\t", ast.var, ast.prefix, pt(ctx))
        if not ctx then --not found : global (easier than to redo all the tests to require declaration ?)
            ast.prefix = - 1
            self:addCode(ast.prefix)
            return self:addCode(- vars[ast.var])
        end
    end
    --print("Compiler:getVariable() 4:\t", ast.var, ast.prefix, pt(ctx))
    self:addCode(ast.prefix)
    return self:addCode(- ctx[ast.var])
--    return self:addCode(assert(rawget(self.vars, ast.var), ("Variable used before definition:\t%s at opCode line:\t%s.\nAST:\t%s"):format(ast.var, #self.code, pt(ast)))
end

--[[
    A function which recursively generates the code of an AST
]]
function Compiler:codeGen(ast)
    return self.switch(ast.tag, self, ast)
end
--[[
    A function which expands the code of `ast[field]`. Returns `self, ast`.
]]
function Compiler:subCodeGen(ast, field)
    if ast[field] == nil then print(([[
        Warning empty field in subCodeGen while parsing %s, looking for field %s.
        It may be anything from a mistake in the parser or the compiler to someone's malpractice with empty statements.
        Hopefully, it is just a missing optional expression.
        ]])
            :format(pt(ast), field))
        return self, ast
    end
    self:codeGen(ast[field])
    return self, ast
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

---@TODO optimizethis as well as Imply (I feel like there must be a way to reduce the length of opCode, the number of jumps in particular.)
---@param self Compiler
---@param ast AST
function Compiler:Xjunction(ast, jmp)
    self:codeGen(ast.exp1)
    local pEnd = self:jmp(jmp)
    self:codeGen(ast.exp2)
    pEnd:honor(#self.code)
end
---@param self Compiler
---@param ast AST
function Compiler:Imply(ast)
    self:codeGen(ast.exp1)
    local pNo = self:jmp'jmp_Z'
    self:codeGen(ast.exp2)
    local pEnd = self:jmp'jmp'
    pNo:honor(#self.code)
    self:addCode'write'
    self:addCode(1)
    pEnd:honor(#self.code)
end

function Compiler:newArray (ast)
    self:subCodeGen(ast, 'size')
    self:addCode'c_new'
    if ast.default then
        --testing whether the size is an integer
        self:addCode'dup'
        self:addCode'push'
        self:addCode(1)
        self:addCode'mod'
        local pInt = self:jmp('jmpop_Z')
        self:addCode'error_array_size'  ---@TODO (after opCode made fully number) do something cleaner than an unknown instruction error. Also clean the stack, you never know when you'll need a protected mode.
        --testing whether size >= 0
        pInt:honor(#self.code)
        self:addCode'dup'
        self:addCode'push'
        self:addCode(0)
        self:addCode'gt'
        local pEnd = self:jmp('jmpop_Z')

        --main loop
        local pLoop = Promise:honored(#self.code)
        self:addCode'up'
        self:subCodeGen(ast, 'default') ---@TODO (after functions and register machine) : fill the array from the bottom up rather than top to bottom.
        self:addCode'c_set'
        self:addCode'push'
        self:addCode(1)
        self:addCode'sub'
        self:jmp('jmp_NZ', pLoop)
        pEnd:honor(#self.code)
    end
    self:addCode'pop'    --popping the index at the end of the loop, to leave the array at the top of the stack
    return self, ast
end


---@return Promise pSize a promise to resolve to the mem size of the bloc (the number of local variables)
function Compiler:blockSize(pSize)
    pSize = pSize or Promise:new()
    self:addCode'block'
    self:addCode(0)
    local pc = #self.code   --saving the current position
    pSize:zen(function (v)
        self.code[pc] = v
    end)
    return pSize
end

--The compiler metatable. contains the metamethods as welle as method related to creation
local metaCompiler = {__name = "Compiler"}
--[[
    A compiler creator.
]]
----@return Compiler
---@param r Compiler
function metaCompiler:new(r)
    --initializing metamethods
    self.__index = rawget(self, '__index') or self
    if rawget(self, '__call') == nil then
        ---@param self Compiler
        function self:__call(ast)
            self:codeGen(ast)
            return self.code
        end
    end
    --initializing data
    r = r or {}
    r.code = r.code or Stack{}

--[[old vars
    r.vars = r.vars or self.vars
    if not r.vars then
        local varsn = Symbol['varsn']
        r.vars = setmetatable({[varsn] = 1},{
            __index = function(vars, key)
                vars[key] = vars[varsn]
--                vars[vars[varsn] ] = key
                vars[varsn] = vars[varsn] + 1
                return vars[key]
            end,
        })
    end
    --A  table of labels and their numerical position
    labels = setmetatable({},{
        __index = function (self, key)
            self[key] = Promise:new()
            return self[key]
        end,
    })
]]

    ---a dictionnary of all variable ids
    ---``` {[name : string] = nameID : number} ```
    r.vars = r.vars or self.vars
    if not r.vars then
        local varsn = Symbol['varsn']
        r.vars = setmetatable({[varsn] = 1},{
            __index = function(vars, key)
                vars[key] = vars[varsn]
    --                vars[vars[varsn] ] = key
                vars[varsn] = vars[varsn] + 1
                return vars[key]
            end,
        })
    end
    print("Compiler:new() 0", pt(r.vars))
    local Cmpctx = {}
    do
        local cmp = self
        local varsn = Symbol['varsn']
        function Cmpctx:__index (key)
            self[key] = self[varsn]
            self[self[varsn] ] = r.vars[key]
            self[varsn] = self[varsn] + 1
            return self[key]
        end
        function Cmpctx:new (t, parent)
            t = t or {}
            t [varsn] = 1
            t.parent = parent or cmp.ctx
            return setmetatable(t, self)
        end
        function Cmpctx:__len() --should be the same as rawlen,
            return self[varsn] - 1
        end
    end

    if not r.ctx then
        r.ctx = Cmpctx:new()
    end

    --initializing the main switch
    setmetatable(r, self)

    ---Adds a single literal line to the code.
    ---@see Compiler.addCode
    local addCode = _COMPILER_DEBUG and
    function (compiler, opCode)
        local t = type(opCode)
        assert(t == 'number' or t == 'string', "addCode : trying to push code other than number:\t" .. pt(t))
        compiler:addCode(opCode)
    end or r.addCode
    local c_addCode = _COMPILER_DEBUG and
    ---Adds a single literal line to the code. Returns `self, ast`, for ease of chaining in the switch.
    ---@see Compiler.c_addCode
    function (compiler, ast, opCode)
        local t = type(opCode)
        assert(t == 'number' or t == 'string' or t == 'table', "addCode : trying to push code other than number or a function pointer:\t" .. pt(t))
        compiler:addCode(opCode)
        return compiler, ast
    end or r.c_addCode
    ---Adds a field verbatim to the code. Returns `self, ast`, for ease of chaining in the switch.
    ---@see Compiler.addCodeField
    local addCodeField = _COMPILER_DEBUG and
    function (compiler, ast, field)
        return c_addCode(compiler, ast, ast[field])
    end or r.addCodeField
    local getVariable = r.getVariable
    ---A function which recursively generates the code of an AST
    ---@see Compiler.codeGen
    local codeGen = r.codeGen
    ---A function which expands the code of `ast[field]`. Returns `self, ast`, for ease of chaining in the switch.
    ---@see Compiler.subCodeGen
    local subCodeGen = r.subCodeGen

    ---@see Compiler.Xjunction
    local Xjunction = r.Xjunction
    ---@see Compiler.Imply
    local Imply = r.Imply
    ---@see Compiler.newArray
    local newArray = r.newArray

    ---expands an assignement, depending on its left hand side's tag.
    ---@TODO split eval of RHS and assignement ; make eval of RHS always happen first.
    ---@type fun(lhs_tag : string, state : Compiler, ast : table): Compiler, table
    --[[@TODO see if you want to keep variables as a disinct lhs from indexed.<br>
    Probably yes, because they can be loaded into a static region of memory and more easily accessed.<br>
    Besides, allocation of custom objects may make things even worse.
    ]]
    r.switch_assign = r.switch_assign or lpeg.Switch{
        ---@TODO rename global ? issue with scopes and overload. Ideally, lhs processed before rhs.
        variable = Cargs(2) * Cc'exp' / subCodeGen * Cc'store' / c_addCode / function(state, ast)
            --state.vars[state.vars[ast.lhs.var]] = ast.lhs.var ; --mark the variable as having an initializer
            --addCode(state, state.vars[ast.lhs.var])
            getVariable(state, ast.lhs, r.vars)
        end * Cargs(2),
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
            subCodeGen(state, ast.lhs, 'ref') ---@TODO (done already?) think about fields, arrays of arrays...
            addCode(state, 'up')
            subCodeGen(state, ast.lhs, 'index')
            addCode(state, 'up')
            subCodeGen(state, ast, 'exp')
            return c_addCode(state, ast, 'set')
        end,
        --print
        io = Cargs(2) * Cc'exp' / subCodeGen * Cc'print' / c_addCode,
        assign = Cargs(2) / function (state, ast)
            subCodeGen(state, ast, 'lhs')
            return codeGen(state, nodeAssign(ast.lhs.lhs, ast.exp))
        end,
        --TODO group and list

        fun = Cargs(2) / function (state, ast)
            codeGen(state, nodeAssign(ast.lhs.ref, Node.nodeNum(r:new()(ast.exp)) ))
        end * Cargs(2),
        [lpeg.Switch.default] = Cargs(2) * r.invalidAst,
    }
    local switch_assign = r.switch_assign

    --Using pattern matching to write code block of sort without having to explicitely write a new function per entry.
    --Sometimes, writing a function is the shortest, especially when using and reuising arguments, but can still easily be incorporated in the pattern (cf. variable)
    --obfuscating ?
    ---@TODO factorize Cargs(2) ? is it possible in any satisfactory way ?
    ---@TODO depreciate state_X
    ---@remark : the switch is put in the new so as to have local functions (addCode, etc.) as well as for inheritance I guess
    r.switch = r.switch or lpeg.Switch{
        ---@TODO depending on how floats are treated, revisit.
        void = lpeg.P(true),
        number = Cargs(2) * Cc'write' / c_addCode * Cc'val' / addCodeField,
        variable = Cargs(2) * Cc'load' / c_addCode * Cc(r.vars) / getVariable,
        -- * ( ((Carg(1) * Cc"vars" / get) * (Carg(2) * Cc"var" / get) / rawget) * Cc"Variable used before definition" / assert / 1 ) / c_addCode,
        indexed = Cargs(2) * Cc'ref' / subCodeGen * Cc'up' / c_addCode * Cc'index' / subCodeGen * Cc'get' / c_addCode,
        fun = Cargs(2) * Cc'ref' / subCodeGen * Cc'call' / c_addCode,
        --I should be able to write newarray higher level. Either in the parser, but it seems more work, or here but it requires function accessing the stack
        new = Cargs(2) / newArray,
--        Array = Cargs(2) * Cc'size' / subCodeGen * Cc'new' / c_addCode, ---only returns a ref to the array, does not not assign it.
        ---@param state Compiler
        block = Cargs(2) / function (state, ast)
            ---@TODO change r.ctx then set it back
            local oldCtx = r.ctx
            r.ctx = Cmpctx:new(nil, oldCtx)
            local p = state:blockSize()
            state:subCodeGen(ast, 'content')
            state:addCode'brek'
            p:honor(#r.ctx)
            r.ctx = oldCtx
        end,
        unaryop = Cargs(2) * Cc'exp' / subCodeGen / codeOP.u / c_addCode,
        binop = Cargs(2) * Cc'exp1' / subCodeGen * Cc'up' / c_addCode * Cc'exp2' / subCodeGen / codeOP.b / c_addCode,
        conjunction = Cargs(2) * Cc'jmp_Z' / Xjunction,
        disjunction = Cargs(2) * Cc'jmp_NZ' / Xjunction,
        imply = Cargs(2) * Cc'jmp_NZ' / Imply,
        compChain = Cargs(2) / function (state, ast)
            codeGen(state, ast.chain[1])
            local pFail = Promise:new()
            for i = 3, #ast.chain - 2, 2 do
                addCode(state, 'up')
                codeGen(state, ast.chain[i])
                addCode(state, r.codeOP.c[ast.chain[i-1]])  --compares while consuming only the first argument
                state:jmp('jmpop_Z', pFail)
            end
            addCode(state, 'up')
            codeGen(state, ast.chain[#ast.chain])
            addCode(state, r.codeOP.b[ast.chain[#ast.chain - 1]])  --compares while consuming both arguments
            if #ast.chain > 3 then
                local pEnd = state:jmp'jmp'
                pFail:honor(#state.code)
                addCode(state,'write')
                addCode(state, 0)
                pEnd:honor(#state.code)
            else    ---@TODO not letting a pending promise out in the open, not sure how necessary or sufficient it is. It probably isn't, to ponder when doing GC with promises.
                ---@TODO probably better, but catch not inplemented yet
                -- pFail.catch() pFail:betray()

                pFail:honor(nil)
            end
        end * Cargs(2),

        assign = Cargs(2) / function (state, ast)
            return switch_assign(ast.lhs.tag, state, ast) --can't use `/ switch_assign` directly because `switch_assign` is actually a table.
        end,

        ['if'] = Cargs(2) * Cc'cond' / subCodeGen / function (state, ast)
            local pEndThen = state:jmp'jmp_Z'
            subCodeGen(state, ast, "then")
            if ast["else"] then
                local pEndIf = state:jmp'jmp'
                pEndThen:honor(#state.code)    --it's ok, the interpreter adds 1
                subCodeGen(state, ast, "else")
                pEndIf:honor(#state.code)
            else
                pEndThen:honor(#state.code)
            end
        end * Cargs(2),
        ['while'] = Cargs(2) / function (state, ast)
            addCode(state, 'push')
            addCode(state, 0)   --yields O if falsy. TODO ? yield cond value instead ? hard with stack machine without dupicating the code generating the expression. Myabe wiht an opInsruction duplicate next ?? With a register should be easy.
            local pStart = Promise:honored(#state.code)
            subCodeGen(state, ast, 'cond')
            local pEnd = state:jmp'jmpop_Z'
            subCodeGen(state, ast, 'stat')
            addCode(state, 'up')
            state:jmp('jmp', pStart)
            pEnd:honor(#state.code)
        end * Cargs(2),
        ['return'] = Cargs(2) * Cc'clean' / c_addCode * Cc'exp' / subCodeGen * Cc'ret' / c_addCode,
        --input
        io = Cargs(2) * Cc'read' / c_addCode,
        ---@param state Compiler
        ---we push the full list, clean what comes after, then moves the head to the start.
        ---I voluntarilly don't clean systemetically after every expression, but only after creation and usage of list.
        ---It's up to the user to not use stack garbage by writing monstrosity such as `a,b = 2 ^ (3,4) * (5 + 6)`
        ---I guess the following could be used to forcefully clean the stack : `a,b = 2 ^ (3,4), * (5 + 6)`
        ---@TODO I guess some warnings and some coercion could be done in the compiler or parser still.
        ---@TODO but yeah, keeping the stack clean is another way to circumvent these issue, but it's not that trivial.
        ---@TODO with a register, it should be aok.
        list = Cargs(2) / function (state, ast)
            for _, e in ipairs(ast.exps) do
                codeGen(state, e)
                addCode(state, 'up')
            end
            addCode(state, 'clean')
            addCode(state, 'mv')
            addCode(state, #ast.exps)
        end * Cargs(2),
        seq = Cargs(2) / function (state, ast) ---@param state Compiler
            for _, s in ipairs(ast.stats) do
                state:codeGen(s)
            end
        end * Cargs(2),
        [lpeg.Switch.default] = Cargs(2) * r.invalidAst,
    }

    return r
end

---The top level __call metamethod also has a check for global variables which are used whitout being ever initialized
function metaCompiler:__call(ast)
    self:codeGen(ast)
--    for k, v in pairs(self.vars) do
--        if type(k) == 'string' and self.vars[v] ~= k then
--            print(("Warning, global variable `%s` is used but never initialized"):format(k))
--        end
--    end
    return self.code
end

metaCompiler:new(Compiler)

--------------------------------------------------------------------------------
return Compiler