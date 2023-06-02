---@TODO : rewrite as a register machine in C++, Kotlin or Rust

local pt = require"pt".pt
local lpeg = require "lpeg"
local Object = require "Object"
local Stack = require"Stack"
local Array = require "Array"
require "utils"
local Context = require"Context"
local VirtualContext = Context.VirtualContext

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

---@TODO : use a register machine for more expressiveness, notably giving the option to implement your left/right/logical-value idea on the dynamic level.
---@TODO : trace with different level of precision and detail : opCodeLine   codeLine   corresponding code
--------------------------------------------------------------------------------
if _INTERPRETER_DEBUG == nil then
    _INTERPRETER_DEBUG = true
end

local function loadCtx(ctx, n, gmem)
    if n == - 1 then
        return gmem
    elseif n >= 0 then
        while n > 0 do
            ctx = ctx.parent
            n = n - 1
        end
        return ctx
    else
        error("loadCtx():\tincorrect prefix")
    end
end

---@class Run
---@field code (number|string)[]
---@field gmem Context
---@field stack VirtualContext
---@field switch fun():boolean?
local Run = {
    ---@TODO on the lua discussion boards, see why concat doesn't use tostring and __tostring and argue/code in favor of it.
    concat = function (t, sep, i, j)
        sep = sep or ', '
        i = i or 1
        j = j or #t
        return table.concat(table.move(t, i, j, 1,
            setmetatable({}, {__newindex = function (self, k, v)
                rawset(self, k, tostring(v):gsub('[\n\t]', ' '))
            end})), sep)
    end
}
local function isFunction(f)
    return type(f) == 'table' and f._fun   ---@TODO to refine
end
function Run:new(code, run)
    run = run or {}
    run.code = code or run.code or self.code or {}
    code = run.code
    run.gmem = run.gmem or self.gmem or {}
    local gmem = run.gmem
    ---@TODO store position as well ? A priori, everything relevant is in the oject stack
    run.stack = run.stack or self.stack or VirtualContext:new({}, Context:new{})
    run.trace = run.trace or Stack() --and nil

    function self:__call(code, run)        
        run = self:new(code, run)
        return run:run()
    end

    do  --putting the most used stuff in local variable for efficiency (going through the inheritance chain only once)
        --the do end block is unecessary, but useful if I ever want to put that piece of code out of the new.

        local pc = 0    --position in code
        local trace = run.trace
        local concat = self.concat
        local stack = run.stack
        --local pStack0 = stack.hpos
        --assert(0<pStack0, "stack length can't be non positive")

        local push, write, pop, peek = stack.parent.push, stack.parent.write, stack.parent.pop, stack.parent.peek
        if trace then
            local rawpush, rawwrite, rawpop, rawpeek = push, write, pop, peek
            ---@param stackParent Context
            ---@param ... ContextElement
            push = function (stackParent, ...)
                --shouldn't happen, if it does, it most likely is an error in the compiler
                if not stackParent.validTypes[type(...)] then 
                    ---@TODO : make it only number/pointers when rewriting the VM.
                    error(("trying to push a value which is neither a number nor an Array:\t%s of type:\t%s"):format(..., type(...)), 2) end
                    trace:push(stackParent.hpos + select('#', ...) .. '  <-- ' .. concat({...}))
                    --trace:push(tostring(stack.parent):gsub('[\n\t]', ' '))
                    rawpush(stackParent, ...)
            end
            ---@param stackParent Context
            ---@param ... ContextElement
            write = function (stackParent, ...)
                --shouldn't happen, if it does, it most likely is an error in the compiler
                if not stackParent.validTypes[type(...)] then 
                    ---@TODO : make it only number/pointers when rewriting the VM.
                    error(("trying to push a value which is neither a number nor an Array:\t%s of type:\t%s"):format(..., type(...)), 2) end
                    trace:push(stackParent.hpos .. '  <>- ' .. concat({...}))
                    --trace:push(tostring(stack.parent):gsub('[\n\t]', ' '))
                    rawwrite(stackParent, ...)
            end
            pop = function (stackParent, n) n = tonumber(n)   --no capture => captures whole match, here ''. so let us tranform it back to nil.
                --shouldn't happen, if it does, it's most likely an error in the compiler.
                n = n or 1 ; assert(n <= stackParent.hpos,
                    ([[trying to pop the stack while empty!
                At opCode line %s
                Trying to pop %s values
                from stack %s
                whose head is at position %s]]):format(tonumber(pc), n, pt(stackParent), stackParent.hpos))
                trace:push(stackParent.hpos .. '  --> ' .. concat(stackParent, nil, stackParent.hpos - n + 1, stackParent.hpos))
                --trace:push(pt(stack.parent):gsub('[\n\t]', ' ') .. '')

                return rawpop(stackParent, n)
            end
            peek = function (stackParent, n) n = tonumber(n)
                --shouldn't happen, if it does, it's most likely an error in the compiler.
                n = n or 1 ; assert(n <= stackParent.hpos,
                    ([[trying to peek the stack while empty!
                At opCode line %s
                Trying to pop %s values
                from stack %s
                whose head is at position %s]]):format(tonumber(pc), n, pt(stackParent), stackParent.hpos))
                trace:push(stackParent.hpos .. '  -<> ' .. concat(stackParent, nil, stackParent.hpos - n + 1, stackParent.hpos))
                return rawpeek(stackParent, n)
            end
        end

        ---@TODO use meta programming to avoid code repetition ?
        ---@TODO use some sort of inheritance to reduce memory ? It increases time because it'd no longer use local, but copying all the below at each fns call seems much more wasteful.
        ---I don't know, with Ultra editing, contiguous one liner code repetition is painless and harmless, and meta programming doesn't help when using lua-language-server
        run.switch = {
            --basic
            push  = function() pc = pc+1 ; push(stack.parent, code[pc]) end,
            write = function() pc = pc+1 ; write(stack.parent, code[pc]) end,
            pop   = function() pop(stack.parent) end,
            up    = function() stack.parent.hpos = stack.parent.hpos + 1 end,  --moves the head one step up the stack
            mv    = function() pc = pc + 1 ; stack.parent.hpos = stack.parent.hpos - code[pc] end, --moves the head a static number of steps down the stack
            mv_d  = function() stack.parent.hpos = stack.parent.hpos - stack.parent.head end,              --moves the head a dynamic number of steps down the stack
            dup   = function() push(stack.parent, peek(stack.parent)) end,
            print = function() print("@ = ", peek(stack.parent)) end,
            read  = function() print"@ " ; write(stack.parent, io.stdin:read('n')) end,
            --control structures
            jmp     = function() pc = pc + 1 ;                          pc = pc + code[pc]     end,
            jmp_Z   = function() pc = pc + 1 ; if peek(stack.parent) == 0 then pc = pc + code[pc] end end,
            jmpop_Z = function() pc = pc + 1 ; if pop (stack.parent) == 0 then pc = pc + code[pc] end end,
            jmp_NZ  = function() pc = pc + 1 ; if peek(stack.parent) ~= 0 then pc = pc + code[pc] end end,
            jmpop_NZ= function() pc = pc + 1 ; if pop (stack.parent) ~= 0 then pc = pc + code[pc] end end,
            --binary operations
            add = function() write(stack.parent, stack.parent[stack.parent.hpos - 1] + pop(stack.parent)) end,
            sub = function() write(stack.parent, stack.parent[stack.parent.hpos - 1] - pop(stack.parent)) end,
            mul = function() write(stack.parent, stack.parent[stack.parent.hpos - 1] * pop(stack.parent)) end,
            div = function() write(stack.parent, stack.parent[stack.parent.hpos - 1] / pop(stack.parent)) end,
            idiv= function() write(stack.parent, stack.parent[stack.parent.hpos - 1] //pop(stack.parent)) end,
            mod = function() write(stack.parent, stack.parent[stack.parent.hpos - 1] % pop(stack.parent)) end,
            pow = function() write(stack.parent, stack.parent[stack.parent.hpos - 1] ^ pop(stack.parent)) end,
            
            --unary operations
            plus = function () end,
            minus  = function() stack.parent.head = - stack.parent.head end,
            ["not"] = function() stack.parent.head = stack.parent.head == 0 and 1 or 0 end,
            --binary comparisons
            lt  = function() write(stack.parent, stack.parent[stack.parent.hpos - 1] <  pop(stack.parent) and 1 or 0) end,
            le  = function() write(stack.parent, stack.parent[stack.parent.hpos - 1] <= pop(stack.parent) and 1 or 0) end,
            gt  = function() write(stack.parent, stack.parent[stack.parent.hpos - 1] >  pop(stack.parent) and 1 or 0) end,
            ge  = function() write(stack.parent, stack.parent[stack.parent.hpos - 1] >= pop(stack.parent) and 1 or 0) end,
            eq  = function() write(stack.parent, stack.parent[stack.parent.hpos - 1] == pop(stack.parent) and 1 or 0) end,
            neq = function() write(stack.parent, stack.parent[stack.parent.hpos - 1] ~= pop(stack.parent) and 1 or 0) end,
            --chained comparisons. if top 2 of the stack.parent are `a, b`, it leaves `b, a comp b`
            c_lt  = function() local a, b = peek(stack.parent, 2) ; stack.parent[stack.parent.hpos - 1] = b; write(stack.parent, a <  b and 1 or 0) end,
            c_le  = function() local a, b = peek(stack.parent, 2) ; stack.parent[stack.parent.hpos - 1] = b; write(stack.parent, a <= b and 1 or 0) end,
            c_gt  = function() local a, b = peek(stack.parent, 2) ; stack.parent[stack.parent.hpos - 1] = b; write(stack.parent, a >  b and 1 or 0) end,
            c_ge  = function() local a, b = peek(stack.parent, 2) ; stack.parent[stack.parent.hpos - 1] = b; write(stack.parent, a >= b and 1 or 0) end,
            c_eq  = function() local a, b = peek(stack.parent, 2) ; stack.parent[stack.parent.hpos - 1] = b; write(stack.parent, a == b and 1 or 0) end,
            c_neq = function() local a, b = peek(stack.parent, 2) ; stack.parent[stack.parent.hpos - 1] = b; write(stack.parent, a ~= b and 1 or 0) end,

            block = function() pc = pc + 1 ; local oldStackParent = stack.parent
                stack.parent = Context:new{memlen = code[pc]}
                stack.parent.parent = oldStackParent
            end,
            ---@TODO ponder a while when reimplementing the vm in a lower level language as a register machine
            ---@TODO ponder 0 or 1 index
            ---@TODO make error when arrlen is not a number
            new = function() stack.parent.head = Context:new({arrlen = tonumber(stack.parent.head)}) end,
            ---@TODO make error when arrlen is not a number
            c_new = function()
                local size = peek(stack.parent)
                stack.parent.head = Context:new({arrlen = tonumber(stack.parent.head)})
                push(stack.parent, size)
            end,
            set = function() local a, k, v = pop(stack.parent, 3)
                assert(type(k) == 'number' and 0 < k and k <= a.arrlen,
                    ("set(Array, ?, ?) : index invalid or out of bound: %s for array %s of size %d"):format(k, a, a.arrlen) )
                --    assert(type(v) or true, "set(Array, ?, ?) : incorrect data type")
                push(stack.parent, set(a, k, v)) end,
            c_set = function() local a, k, v = peek(stack.parent, 3);
                assert(type(k) == 'number' and 0 < k and k <= a.arrlen,
                    ("set(Array, ?, ?) : index invalid or out of bound: %s for array %s of size %d"):format(k, a, a.arrlen) )
                --    assert(type(v) or true, "set(Array, ?, ?) : incorrect data type")
                set(a, k, v) ; stack.parent.hpos = stack.parent.hpos - 1 end,
            ---@TODO think of default value. is it nil, is it garbage ? do we keep it as an error ?
            get = function ()
                assert(0 < stack.parent.head and stack.parent.head <= stack.parent[stack.parent.hpos-1].arrlen,
                    ("Array index (%s) out of range (%s)"):format(stack.parent.head, stack.parent[stack.parent.hpos-1].arrlen))
                push(stack.parent, rawget(pop(stack.parent, 2)))
            end,
            clean = function () stack.parent:clean() end,
            load  = function()
                pc = pc+1
                local ctx = loadCtx(stack.parent, code[pc], gmem)
                pc = pc + 1
                write(stack.parent, ctx[code[pc]])
            end,
            store = function()
                pc = pc+1
                local ctx = loadCtx(stack.parent, code[pc], gmem)
                pc = pc + 1
                ctx[code[pc]] = peek(stack.parent)
            end,
            brek  = function()
                assert(stack.parent.hpos == 1, "Incorrect Stack head position upon break:\n" .. tostring(stack.parent) .. "\t len:\t" .. stack.parent.hpos)
                if not stack.parent.parent then
                    --print("toplevel")
                    return true
                end
                write(stack.parent.parent, (stack.parent[0]))
                stack.parent = stack.parent.parent
            end,
            ret   = function()
                assert(stack.parent.hpos == 1, "Incorrect Stack head position upon return:\n" .. tostring(stack.parent) .. "\t len:\t" .. stack.parent.hpos)
                if not stack.parent.parent then
                    --print("toplevel")
                    return true
                end
                if rawlen(stack.parent) == 0 then
                    stack.parent.parent:clean()
                return true
                end
                local s = stack.parent.parent.hpos
                if s == 0 then
                    print("ret: warning head position was zero")
                    s = 1
                end
                stack.parent.parent.hpos = s - 1
                stack.parent.parent:push(table.unpack(stack.parent, 1, rawlen(stack.parent)))
                stack.parent.parent.hpos = s
                return true
            end,
            bindFunc = function ()  ---@TODO : split into copy and bind, and use bind for other shenanigans ?
---@diagnostic disable-next-line: param-type-mismatch
                stack.parent.head = stack.parent.head:cpy()
                stack.parent.head[0] = stack.parent
            end,
            call  = function()
                assert(isFunction(stack.parent.head), "call: not a function:\t" .. tostring(stack.parent.head))

---@diagnostic disable-next-line: param-type-mismatch
                run(stack.parent.head:
                    cpy())
            end, ---@TODO TODO
        }
        setmetatable(run.switch, {__index = function()
                print(trace and trace:unpack())
                --should not be happening, if it does, there most likely is an error in the compiler.
                ---@TODO output in stderr. for now I use stdout for readibility, because apparently I have no control on how stdout and stderr will mix on the terminal output.
                io.stdout:write("unknown instruction:\t" .. code[pc] .. " at line:\t" .. tostring(pc) .. " of code:\t" .. pt(code) .. "\n")   --lpeg match catches errors I think
                os.exit(false)
            end,
            __call = trace and function (self)
                pc = pc + 1
                if _INTERPRETER_DEBUG then  print(trace:unpack()) end ---@TODO make better
                trace = Stack{tostring(pc) .. "\tinstruction: " .. code[pc]}    --TODO : print trace and program outpout to different streams ?
                return self[code[pc]]()
            end or function (self)
                pc = pc + 1
                return self[code[pc]]()
            end
        })
    end
    self.__index = self
    return setmetatable(run, self)
end
function Run:run()
    repeat until self.switch()
    local r = self.stack.parent
    return r
end
--------------------------------------------------------------------------------

return Run:new()