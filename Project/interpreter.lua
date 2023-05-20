---@TODO : rewrite as a register machine in C++, Kotlin or Rust

local pt = require"pt".pt
local lpeg = require "lpeg"
local Object = require "Object"
local Stack = require"Stack"
local Array = require "Array"
require "utils"
local IntStack = require"IntStack"

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
---@class Run
---@field code (number|string)[]
---@field mem table
---@field stack IntStack
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
function Run:new(code, run)
    run = run or {}
    run.code = code or run.code or self.code or {}
    code = run.code
    run.mem = run.mem or self.mem or {}
    local mem = run.mem
    ---@TODO store position as well ? A priori, everything relevant is in the oject stack
    run.stack = run.stack or self.stack or IntStack:new()
    run.trace = run.trace or Stack() --and nil

    function self:__call(code, run)
        print("Run:new()", pt(code), run)
        
        --print("run.__call()", pt(code), run)
        run = self:new(code, run)
        return run:run()
    end

    do  --putting the most used stuff in local variable for efficiency (going through the inheritance chain only once)
        --the do end block is unecessary, but useful if I ever want to put that piece of code out of the new.

        local pc = 0    --position in code
        local trace = run.trace
        local concat = self.concat
        local stack = run.stack
        local pStack0 = #stack
        assert(0<pStack0, "stack length can't be non positive")

        local push, write, pop, peek = stack.push, stack.write, stack.pop, stack.peek
        if trace then
            local rawpush, rawwrite, rawpop, rawpeek = push, write, pop, peek
            ---@param stack IntStack
            ---@param ... IntStackElement
            push = function (stack, ...)
                --shouldn't happen, if it does, it most likely is an error in the compiler
                if not stack.validTypes[type(...)] then error(("trying to push a value which is neither a number nor an Array:\t%s of type:\t%s"):format(..., type(...)), 2) end ---@TODO : make it only number/pointers when rewriting the VM.
                trace:push(#stack + select('#', ...) .. '  <-- ' .. concat({...}))
                --trace:push(tostring(stack):gsub('[\n\t]', ' '))
                rawpush(stack, ...)
            end
            ---@param stack IntStack
            ---@param ... IntStackElement
            write = function (stack, ...)
                --shouldn't happen, if it does, it most likely is an error in the compiler
                if not stack.validTypes[type(...)] then error(("trying to push a value which is neither a number nor an Array:\t%s of type:\t%s"):format(..., type(...)), 2) end ---@TODO : make it only number/pointers when rewriting the VM.
                trace:push(#stack .. '  <>- ' .. concat({...}))
                --trace:push(tostring(stack):gsub('[\n\t]', ' '))
                rawwrite(stack, ...)
            end
            pop = function (stack, n) n = tonumber(n)   --no capture => captures whole match, here ''. so let us tranform it back to nil.
                --shouldn't happen, if it does, it's most likely an error in the compiler.
                n = n or 1 assert(n <= #stack, ([[trying to pop the stack while empty!
                At opCode line %s
                Trying to pop %s values
                from stack %s]]):format(tonumber(pc), n, pt(stack)))
                trace:push(#stack .. '  --> ' .. concat(stack, nil, #stack - n + 1))
                --trace:push(pt(stack):gsub('[\n\t]', ' ') .. '')

                return rawpop(stack, n)
            end
            peek = function (stack, n) n = tonumber(n)
                --shouldn't happen, if it does, it's most likely an error in the compiler.
                n = n or 1 assert(n <= #stack, ([[trying to peek the stack while empty!
            At opCode line %s
            Trying to pop %s values
            from stack %s]]):format(tonumber(pc), n, pt(stack)))
                trace:push(#stack .. '  -<> ' .. concat(stack, nil, #stack - n + 1))
                return rawpeek(stack, n)
            end
        end

        ---@TODO use meta programming to avoid code repetition ?
        ---@TODO use some sort of inheritance to reduce memory ? It increases time because it'd no longer use local, but copying all the below at each fns call seems much more wasteful.
        ---I don't know, with Ultra editing, contiguous one liner code repetition is painless and harmless, and meta programming doesn't help when using lua-language-server
        run.switch = {
            --basic
            push  = function() pc = pc+1 ; push(stack, code[pc]) end,
            write = function() pc = pc+1 ; write(stack, code[pc]) end,
            pop   = function() pop(stack) end,
            up    = function() stack.len = #stack + 1 ; end,  --moves the head one step up the stack
            mv    = function() pc = pc + 1 ; stack.len = #stack - code[pc] end, --moves the head a static number of steps down the stack
            mv_d  = function() stack.len = #stack - stack.top end,              --moves the head a dynamic number of steps down the stack
            dup   = function() push(stack, peek(stack)) end,
            load  = function() pc = pc+1 ; write(stack, mem[code[pc]]) end,
            store = function() pc = pc+1 ; mem[code[pc]] = peek(stack) end,
            print = function() print("@ = ", peek(stack)) end,
            read  = function() print"@ " ; write(stack, io.stdin:read('n')) end,
            ret   = function() return assert(#stack == pStack0, "Incorrect Stack height upon return:\n" .. tostring(stack) .. "\t len:\t" .. #stack .. "\t pStack0:\t" .. pStack0) end,
            call  = function() print("call0", run.code, pc) ; run(stack.top); print("call1", run.code, pc) ; return true end,
            --control structures
            jmp     = function() pc = pc + 1 ;                          pc = pc + code[pc]     end,
            jmp_Z   = function() pc = pc + 1 ; if peek(stack) == 0 then pc = pc + code[pc] end end,
            jmpop_Z = function() pc = pc + 1 ; if pop (stack) == 0 then pc = pc + code[pc] end end,
            jmp_NZ  = function() pc = pc + 1 ; if peek(stack) ~= 0 then pc = pc + code[pc] end end,
            jmpop_NZ= function() pc = pc + 1 ; if pop (stack) ~= 0 then pc = pc + code[pc] end end,
            --binary operations
            add = function() write(stack, stack[-1] + pop(stack)) end,
            sub = function() write(stack, stack[-1] - pop(stack)) end,
            mul = function() write(stack, stack[-1] * pop(stack)) end,
            div = function() write(stack, stack[-1] / pop(stack)) end,
            mod = function() write(stack, stack[-1] % pop(stack)) end,
            pow = function() write(stack, stack[-1] ^ pop(stack)) end,
--        ["and"] = function() if stack[-1] == 0 then stack.top = pop(stack) else stack.len = #stack - 1 end end,
--        ["or"]  = function() if stack[-1] ~= 0 then stack.top = pop(stack) else stack.len = #stack - 1 end end,
            --unary operations
            plus = function () end,
            minus  = function() stack.top = -stack[0] end,
            ["not"] = function() stack.top = stack.top == 0 and 1 or 0 end,
            --binary comparisons
            lt  = function() write(stack, stack[-1] <  pop(stack) and 1 or 0) print("lt", stack) end,
            le  = function() write(stack, stack[-1] <= pop(stack) and 1 or 0) end,
            gt  = function() write(stack, stack[-1] >  pop(stack) and 1 or 0) end,
            ge  = function() write(stack, stack[-1] >= pop(stack) and 1 or 0) end,
            eq  = function() write(stack, stack[-1] == pop(stack) and 1 or 0) end,
            neq = function() write(stack, stack[-1] ~= pop(stack) and 1 or 0) end,
            --chained comparisons. if top 2 of the stack are `a, b`, it leaves `b, a comp b`
            c_lt  = function() local a, b = peek(stack, 2) ; stack[-1] = b; write(stack, a <  b and 1 or 0) end,
            c_le  = function() local a, b = peek(stack, 2) ; stack[-1] = b write(stack, a <= b and 1 or 0)  end,
            c_gt  = function() local a, b = peek(stack, 2) ; stack[-1] = b; write(stack, a >  b and 1 or 0) end,
            c_ge  = function() local a, b = peek(stack, 2) ; stack[-1] = b; write(stack, a >= b and 1 or 0) end,
            c_eq  = function() local a, b = peek(stack, 2) ; stack[-1] = b; write(stack, a == b and 1 or 0) end,
            c_neq = function() local a, b = peek(stack, 2) ; stack[-1] = b; write(stack, a ~= b and 1 or 0) end,

            --arrays
            ---for now, arrays are simply lua tables

            ---@TODO ponder a while when reimplementing the vm in a lower level language as a registr machine
            ---@TODO ponder 0 or 1 index
            ---@diagnostic disable-next-line: param-type-mismatch
            new = function() stack.top = Array(stack.top) end,
            ---@diagnostic disable-next-line: param-type-mismatch
            c_new = function() local size = peek(stack) ; stack.top = Array(size) ; push(stack, size) end,
            set = function() local a, k, v = pop(stack, 3);
                assert(type(k) == 'number' and 0 < k and k <= #a, ("set(Array, ?, ?) : index invalid or out of bound: %s for array %s of size %d"):format(k, a, #a) )
                --    assert(type(v) or true, "set(Array, ?, ?) : incorrect data type")
                push(stack, set(a, k, v)) end,
            c_set = function() local a, k, v = peek(stack, 3);
                assert(type(k) == 'number' and 0 < k and k <= #a, ("set(Array, ?, ?) : index invalid or out of bound: %s for array %s of size %d"):format(k, a, #a) )
                --    assert(type(v) or true, "set(Array, ?, ?) : incorrect data type")
                set(a, k, v) ; stack.len = #stack - 1 end,
            ---@TODO think of default value. is it nil, is it garbage ? do we keep it as an error ?
            get = function () assert(0 < stack.top and stack.top <= #stack[-1], "Array index out of range") ; push(stack, rawget(pop(stack, 2))) end,
            clean = function () stack:clean() end,
            pack = function () stack:pack() end,
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
                print(trace:unpack())
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
    local r =  self.stack.top
    return r
end
--setmetatable(Run, {__call = function (self, code, run)
--    run = Run:new(code, run)
--    return run:run()
--end})
--------------------------------------------------------------------------------

return Run:new()