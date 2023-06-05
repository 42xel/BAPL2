---@TODO : rewrite as a register machine in C++, Kotlin or Rust

local pt = require"pt".pt
local lpeg = require "lpeg"
local Object = require "Object"
local Stack = require"Stack"
local Array = require "Array"
require "utils"
local Proxy = require"Proxy"
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
    local oldCtx, oldN = ctx, n
    if n == 0 then
        return gmem
    elseif n > 0 then
        while n > 0 do
            ctx = ctx.parent
            n = n - 1
            if not ctx then
                error(("prefix %s invalid at runtime in Context %s\t%s"):format(oldN, oldCtx, pt(oldCtx)), 2)
            end
        end
        return ctx
    elseif n < 0 then
        while n < 0 do
            ctx = ctx.caller
            n = n + 1
            if not ctx then
                error(("prefix %s invalid at runtime in Context %s\t%s"):format(oldN, oldCtx, pt(oldCtx)), 2)
            end
        end
        return ctx
    else
        error("loadCtx():\tincorrect prefix " .. oldN)
    end
end

---@class Run
---@field code (number|string)[]
---@field gmem Context
---@field vctx VirtualContext
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

    run.vctx = run.vctx or self.vctx and VirtualContext:new{vtcxParent = self.vctx} or
        VirtualContext:new{
            stack = Context:new{}
        }
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
        local vctx = run.vctx
        --local pStack0 = vctx.ihpos
        --assert(0<pStack0, "stack length can't be non positive")

        local push, write, pop, peek = vctx.stack.push, vctx.stack.write, vctx.stack.pop, vctx.stack.peek
        if trace then
            local rawpush, rawwrite, rawpop, rawpeek = push, write, pop, peek
            ---@param stackCaller Context
            ---@param ... ContextElement
            push = function (stackCaller, ...)
                --shouldn't happen, if it does, it most likely is an error in the compiler
                if not stackCaller.validTypes[type(...)] then 
                    ---@TODO : make it only number/pointers when rewriting the VM.
                    error(("trying to push a value which is neither a number nor an Array:\t%s of type:\t%s"):format(..., type(...)), 2) end
                    trace:push(stackCaller.hpos + select('#', ...) .. '  <-- ' .. concat({...}))
                    --trace:push(tostring(stack.parent):gsub('[\n\t]', ' '))
                    rawpush(stackCaller, ...)
            end
            ---@param stackCaller Context
            ---@param ... ContextElement
            write = function (stackCaller, ...)
                --shouldn't happen, if it does, it most likely is an error in the compiler
                if not stackCaller.validTypes[type(...)] then 
                    ---@TODO : make it only number/pointers when rewriting the VM.
                    error(("trying to push a value which is neither a number nor an Array:\t%s of type:\t%s"):format(..., type(...)), 2) end
                    trace:push(stackCaller.hpos .. '  <>- ' .. concat({...}))
                    --trace:push(tostring(stackCaller):gsub('[\n\t]', ' '))
                    rawwrite(stackCaller, ...)
            end
            pop = function (stackCaller, n) n = tonumber(n)   --no capture => captures whole match, here ''. so let us tranform it back to nil.
                --shouldn't happen, if it does, it's most likely an error in the compiler.
                n = n or 1 ; assert(n <= stackCaller.hpos,
                    ([[trying to pop the stack while empty!
                At opCode line %s
                Trying to pop %s values
                from stack %s
                whose head is at position %s]]):format(tonumber(pc), n, pt(stackCaller), stackCaller.hpos))
                trace:push(stackCaller.hpos .. '  --> ' .. concat(stackCaller, nil, stackCaller.hpos - n + 1, stackCaller.hpos))
                --trace:push(pt(stackCaller):gsub('[\n\t]', ' ') .. '')

                return rawpop(stackCaller, n)
            end
            peek = function (stackCaller, n) n = tonumber(n)
                --shouldn't happen, if it does, it's most likely an error in the compiler.
                n = n or 1 ; assert(n <= stackCaller.hpos,
                    ([[trying to peek the stack while empty!
                At opCode line %s
                Trying to pop %s values
                from stack %s
                whose head is at position %s]]):format(tonumber(pc), n, pt(stackCaller), stackCaller.hpos))
                trace:push(stackCaller.hpos .. '  -<> ' .. concat(stackCaller, nil, stackCaller.hpos - n + 1, stackCaller.hpos))
                return rawpeek(stackCaller, n)
            end
        end

        ---@TODO use meta programming to avoid code repetition ?
        ---@TODO use some sort of inheritance to reduce memory ? It increases time because it'd no longer use local, but copying all the below at each fns call seems much more wasteful.
        ---I don't know, with Ultra editing, contiguous one liner code repetition is painless and harmless, and meta programming doesn't help when using lua-language-server
        run.switch = {
            --basic
            push  = function() pc = pc+1 ; push(vctx.stack, code[pc]) end,
            write = function() pc = pc+1 ; write(vctx.stack, code[pc]) end,
            pop   = function() pop(vctx.stack) end,
            up    = function() vctx.stack.hpos = vctx.stack.hpos + 1 end,  --moves the head one step up the stack
            mv    = function() pc = pc + 1 ; vctx.stack.hpos = vctx.stack.hpos - code[pc] end, --moves the head a static number of steps down the stack
            mv_d  = function() vctx.stack.hpos = vctx.stack.hpos - vctx.stack.head end,              --moves the head a dynamic number of steps down the stack
            dup   = function() push(vctx.stack, peek(vctx.stack)) end,
            print = function() print("@ = ", peek(vctx.stack)) end,
            read  = function() print"@ " ; write(vctx.stack, io.stdin:read('n')) end,
            --control structures
            jmp     = function() pc = pc + 1 ;                          pc = pc + code[pc]     end,
            jmp_Z   = function() pc = pc + 1 ; if peek(vctx.stack) == 0 then pc = pc + code[pc] end end,
            jmpop_Z = function() pc = pc + 1 ; if pop (vctx.stack) == 0 then pc = pc + code[pc] end end,
            jmp_NZ  = function() pc = pc + 1 ; if peek(vctx.stack) ~= 0 then pc = pc + code[pc] end end,
            jmpop_NZ= function() pc = pc + 1 ; if pop (vctx.stack) ~= 0 then pc = pc + code[pc] end end,
            --binary operations
            add = function() write(vctx.stack, vctx.stack[vctx.stack.hpos - 1] + pop(vctx.stack)) end,
            sub = function() write(vctx.stack, vctx.stack[vctx.stack.hpos - 1] - pop(vctx.stack)) end,
            mul = function() write(vctx.stack, vctx.stack[vctx.stack.hpos - 1] * pop(vctx.stack)) end,
            div = function() write(vctx.stack, vctx.stack[vctx.stack.hpos - 1] / pop(vctx.stack)) end,
            idiv= function() write(vctx.stack, vctx.stack[vctx.stack.hpos - 1] //pop(vctx.stack)) end,
            mod = function() write(vctx.stack, vctx.stack[vctx.stack.hpos - 1] % pop(vctx.stack)) end,
            pow = function() write(vctx.stack, vctx.stack[vctx.stack.hpos - 1] ^ pop(vctx.stack)) end,
            
            --unary operations
            plus = function () end,
            minus  = function() vctx.stack.head = - vctx.stack.head end,
            ["not"] = function() vctx.stack.head = vctx.stack.head == 0 and 1 or 0 end,
            --binary comparisons
            lt  = function() write(vctx.stack, vctx.stack[vctx.stack.hpos - 1] <  pop(vctx.stack) and 1 or 0) end,
            le  = function() write(vctx.stack, vctx.stack[vctx.stack.hpos - 1] <= pop(vctx.stack) and 1 or 0) end,
            gt  = function() write(vctx.stack, vctx.stack[vctx.stack.hpos - 1] >  pop(vctx.stack) and 1 or 0) end,
            ge  = function() write(vctx.stack, vctx.stack[vctx.stack.hpos - 1] >= pop(vctx.stack) and 1 or 0) end,
            eq  = function() write(vctx.stack, vctx.stack[vctx.stack.hpos - 1] == pop(vctx.stack) and 1 or 0) end,
            neq = function() write(vctx.stack, vctx.stack[vctx.stack.hpos - 1] ~= pop(vctx.stack) and 1 or 0) end,
            --chained comparisons. if top 2 of the stack.caller are `a, b`, it leaves `b, a comp b`
            c_lt  = function() local a, b = peek(vctx.stack, 2) ; vctx.stack[vctx.stack.hpos - 1] = b; write(vctx.stack, a <  b and 1 or 0) end,
            c_le  = function() local a, b = peek(vctx.stack, 2) ; vctx.stack[vctx.stack.hpos - 1] = b; write(vctx.stack, a <= b and 1 or 0) end,
            c_gt  = function() local a, b = peek(vctx.stack, 2) ; vctx.stack[vctx.stack.hpos - 1] = b; write(vctx.stack, a >  b and 1 or 0) end,
            c_ge  = function() local a, b = peek(vctx.stack, 2) ; vctx.stack[vctx.stack.hpos - 1] = b; write(vctx.stack, a >= b and 1 or 0) end,
            c_eq  = function() local a, b = peek(vctx.stack, 2) ; vctx.stack[vctx.stack.hpos - 1] = b; write(vctx.stack, a == b and 1 or 0) end,
            c_neq = function() local a, b = peek(vctx.stack, 2) ; vctx.stack[vctx.stack.hpos - 1] = b; write(vctx.stack, a ~= b and 1 or 0) end,

            ---@TODO ponder a while when reimplementing the vm in a lower level language as a register machine
            ---@TODO ponder 0 or 1 index
            ---@TODO make error when arrlen is not a number
            new = function() vctx.stack.head = Context:new({arrlen = tonumber(vctx.stack.head)}) end,
            ---@TODO make error when arrlen is not a number
            c_new = function()
                local size = peek(vctx.stack)
                vctx.stack.head = Context:new({arrlen = tonumber(vctx.stack.head)})
                push(vctx.stack, size)
            end,
            set = function() local a, k, v = pop(vctx.stack, 3)
                assert(type(k) == 'number' and 0 < k and k <= a.arrlen,
                    ("set(Array, ?, ?) : index invalid or out of bound: %s for array %s of size %d"):format(k, a, a.arrlen) )
                --    assert(type(v) or true, "set(Array, ?, ?) : incorrect data type")
                push(vctx.stack, set(a, k, v)) end,
            c_set = function() local a, k, v = peek(vctx.stack, 3);
                assert(type(k) == 'number' and 0 < k and k <= a.arrlen,
                    ("set(Array, ?, ?) : index invalid or out of bound: %s for array %s of size %d"):format(k, a, a.arrlen) )
                --    assert(type(v) or true, "set(Array, ?, ?) : incorrect data type")
                set(a, k, v) ; vctx.stack.hpos = vctx.stack.hpos - 1 end,
            ---@TODO think of default value. is it nil, is it garbage ? do we keep it as an error ?
            get = function ()
                assert(0 < vctx.stack.head and vctx.stack.head <= vctx.stack[vctx.stack.hpos-1].arrlen,
                    ("Array index (%s) out of range (%s)"):format(vctx.stack.head, vctx.stack[vctx.stack.hpos-1].arrlen))
                push(vctx.stack, rawget(pop(vctx.stack, 2)))
            end,
            clean = function () vctx.stack:clean() end,
            load  = function()
                pc = pc+1
                local ctx = loadCtx(vctx, code[pc], gmem)
                pc = pc + 1
                write(vctx.stack, ctx[code[pc]])
            end,
            store = function()
                pc = pc+1
                local ctx = loadCtx(vctx, code[pc], gmem)
                pc = pc + 1
                ctx[code[pc]] = peek(vctx.stack)
            end,
            block = function() pc = pc + 1
                local newStack = Context:new{
                    memlen = code[pc],
                    parent = vctx.parent,
                    --caller = vctx.caller,
                }
                vctx = VirtualContext:new{
                    vtcxParent = vctx,
                    stack = newStack,
                    parent = newStack,
                }
            end,
            brek  = function()
                assert(vctx.ihpos == vctx.stack.hpos, "Incorrect Stack head position upon break:\n" .. tostring(vctx.parent) .. "\t len:\t" .. vctx.parent.hpos)
                if not vctx.vtcxParent then
                    --print("toplevel")
                    return true
                end
                --litteral array declaration : we set the size at the end.
                ---@TODO In a lower level VM, there might be a copy, or a conversion from Vector (C++) to array, or not.
                if vctx.stack[0] == vctx.stack and vctx.stack.arrlen == 0 then
                    vctx.stack.arrlen = rawlen(vctx.stack)
                end
                write(vctx.vtcxParent.stack, (vctx.stack[0]))
                vctx = vctx.vtcxParent
            end,
            fundef = function ()  ---@TODO : split into copy and bind, and use bind for other shenanigans ?
                pc = pc + 1
                vctx.parent.head = Context.pxy(code[pc], { --turning the static definition into a dynamic object
                    [0] = vctx.parent.head,        --default value
                    _fun = true,                   --to differentiate between arrays and functions.
                    parent = vctx.parent,
                })
                --print("fundef", vctx.parent.head, vctx.parent.head.arrlen, vctx.parent.head[1])
            end,
            call  = function()
                local param = vctx.parent.head
                vctx.parent.hpos = vctx.parent.hpos - 1

                assert(isFunction(vctx.parent.head), "call: not a function:\t" .. tostring(vctx.parent.head))
---@diagnostic disable-next-line: param-type-mismatch
                local fni = vctx.parent.head:pxy{   --
                    [0] = param,
                    caller = code,-- vctx.caller,
                }
                run(fni, {
                    vctx = VirtualContext:new{
                        vtcxParent = vctx,
                        parent = vctx.parent.head.parent,
                        caller = fni,
                    },
                })
            end,
            ret   = function()
                assert(vctx.ihpos == vctx.stack.hpos, "Incorrect Stack head position upon return:\n" .. tostring(vctx.parent) .. "\t len:\t" .. vctx.parent.hpos)
                if not vctx.vtcxParent then
                    --print("toplevel")
                    return true
                end

                local s = vctx.vtcxParent.stack.hpos
                if s == 0 then
                    print("ret: warning head position was zero")
                    s = 1
                end
                vctx.vtcxParent.stack.hpos = s - 1
                vctx.vtcxParent.stack:push(table.unpack(vctx.stack, vctx.ihpos, rawlen(vctx.stack)))
                vctx.vtcxParent.stack.hpos = vctx.vtcxParent.stack.hpos + 1
                vctx.vtcxParent.stack:clean() -- doesn't beacuse it's a proxy ? or because up is executed at the wrong (virtual) stack ?
                vctx.vtcxParent.stack.hpos = s

                return true
            end,
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
                --print("run:__call", code, pt(code))
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
    local r = self.vctx.stack
    return r
end
--------------------------------------------------------------------------------

return Run:new()