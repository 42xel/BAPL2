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
function Run:new(code, mem, run)
    run = run or {}
    run.code = code or run.code or self.code or {}
    code = run.code
    run.mem = mem or run.mem or self.mem or {}
    run.stack = run.stack or IntStack:new()
    run.trace = run.trace or Stack() --and nil

    do  --putting the most used stuff in local variable for efficiency (going through the inheritance chain only once)
        --the do end block is unecessary, but useful if I ever want to put that piece of code out of the new.

        local pc = 0    --position in code
        local trace = run.trace
        local concat = self.concat
        local stack = run.stack
        local meme = run.mem

        local push, pop, peek = stack.push, stack.pop, stack.peek
        if trace then
            local rawpush, rawpop, rawpeek = push, pop, peek
            push = function (self, ...)
                --shouldn't happen, if it does, it most likely is an error in the compiler
                if type(...) ~= 'number' and type(...) ~= 'table' then error(("trying to push a value which is neither a number nor an Array:\t%s of type:\t%s"):format(..., type(...)) ) end ---@TODO : make it only number/pointers when rewriting the VM.
                trace:push('<- ' .. concat({...}))
                --trace:push(tostring(stack):gsub('[\n\t]', ' '))
                rawpush(self, ...)
            end
            pop = function (n) n = tonumber(n)   --no capture => captures whole match, here ''. so let us tranform it back to nil.
                --shouldn't happen, if it does, it's most likely an error in the compiler.
                n = n or 1 assert(n <= #stack, ([[trying to pop the stack while empty!
                At opCode line %s
                Trying to pop %s values
                from stack %s]]):format(tonumber(pc), n, pt(stack)))
                trace:push('-> ' .. concat(stack, nil, #stack - n + 1))
                --trace:push(pt(stack):gsub('[\n\t]', ' ') .. '')
                return rawpop(self, n)
            end
            peek = function (n) n = tonumber(n)
                --shouldn't happen, if it does, it's most likely an error in the compiler.
                n = n or 1 assert(n <= #stack, ([[trying to pop the stack while empty!
            At opCode line %s
            Trying to pop %s values
            from stack %s]]):format(tonumber(pc), n, pt(stack)))
                return rawpeek(self, n)
            end
        end
        local write = stack.write

        ---@TODO use meta programming to avoid code repetition ?
        ---I don't know, with Ultra editing, contiguous one liner code repetition is painless and harmless, and meta programming doesn't help when using lua-language-server
        Run.switch = {
            --basic
            push  = function() pc = pc+1 push(stack, code[pc]) end,
            write = function() pc = pc+1 write(stack, code[pc]) end,
            pop   = function() pop(stack) end,
            dup   = function() push(stack, peek(stack)) end,
            load  = function() pc = pc+1 push(stack, mem[code[pc]]) end,
            store = function() pc = pc+1 mem[code[pc]] = pop(stack) end,
            print = function() print("@", pop(stack)) end,
            ret   = function() return assert(#stack == 1, "Incorrect Stack height upon return:\n" .. tostring(stack) .. "\t len:\t" .. #stack) end,
            --control structures
            jmp     = function() pc = pc + 1 ;                          pc = pc + code[pc]     end,
            jmp_Z   = function() pc = pc + 1 ; if peek(stack) == 0 then pc = pc + code[pc] end end,
            jmpop_Z = function() pc = pc + 1 ; if pop (stack) == 0 then pc = pc + code[pc] end end,
            jmp_NZ  = function() pc = pc + 1 ; if peek(stack) ~= 0 then pc = pc + code[pc] end end,
            jmpop_NZ= function() pc = pc + 1 ; if pop (stack) ~= 0 then pc = pc + code[pc] end end,
            --binary operations
            add = function() stack.top = stack[-1] + pop(stack) end,
            sub = function() stack.top = stack[-1] - pop(stack) end,
            mul = function() stack.top = stack[-1] * pop(stack) end,
            div = function() stack.top = stack[-1] / pop(stack) end,
            mod = function() stack.top = stack[-1] % pop(stack) end,
            pow = function() stack.top = stack[-1] ^ pop(stack) end,
--        ["and"] = function() if stack[-1] == 0 then stack.top = pop(stack) else stack.len = #stack - 1 end end,
--        ["or"]  = function() if stack[-1] ~= 0 then stack.top = pop(stack) else stack.len = #stack - 1 end end,
            --unary operations
            plus = function () end,
            minus  = function() stack.top = -stack[0] end,
            ["not"] = function() stack.top = stack.top == 0 and 1 or 0 end,
            --binary comparisons
            lt  = function() stack.top = stack[-1] <  pop(stack) and 1 or 0 end,
            le  = function() stack.top = stack[-1] <= pop(stack) and 1 or 0 end,
            gt  = function() stack.top = stack[-1] >  pop(stack) and 1 or 0 end,
            ge  = function() stack.top = stack[-1] >= pop(stack) and 1 or 0 end,
            eq  = function() stack.top = stack[-1] == pop(stack) and 1 or 0 end,
            neq = function() stack.top = stack[-1] ~= pop(stack) and 1 or 0 end,
            --chained comparisons. if top 2 of the stack are `a, b`, it leaves `b, a comp b`
            c_lt  = function() local b = stack.top ; stack.top = stack.top <  b and 1 or 0 stack[-1] = b end,
            c_le  = function() local b = stack.top ; stack.top = stack.top <= b and 1 or 0 stack[-1] = b end,
            c_gt  = function() local b = stack.top ; stack.top = stack.top >  b and 1 or 0 stack[-1] = b end,
            c_ge  = function() local b = stack.top ; stack.top = stack.top >= b and 1 or 0 stack[-1] = b end,
            c_eq  = function() local b = stack.top ; stack.top = stack.top == b and 1 or 0 stack[-1] = b end,
            c_neq = function() local b = stack.top ; stack.top = stack.top ~= b and 1 or 0 stack[-1] = b end,

            --arrays
            ---for now, arrays are simply lua tables

            ---@TODO ponder a while when reimplementing the vm in a lower level language as a registr machine
            ---@TODO ponder 0 or 1 index
            ---@diagnostic disable-next-line: param-type-mismatch
            new = function() stack.top = Array(stack.top) end,
            ---@diagnostic disable-next-line: param-type-mismatch
            c_new = function() local size = stack.top ; stack.top = Array(size) ; push(stack, size) end,
            set = function() push(stack, set(pop(stack, 3))) end,
            c_set = function() set(peek(stack, 3)) stack.len = #stack - 1 end,
            ---@TODO think of default value. is it nil, is it garbage ? do we keep it as an error ?
            get = function () stack.top = assert(rawget(pop(stack, 2)), "Array index out of range") end,
        }
        setmetatable(Run.switch, {__index = function()
                print(trace and trace:unpack())
                --should not be happening, if it does, there most likely is an error in the compiler.
                ---@TODO output in stderr. for now I use stdout for readibility, because apparently I have no control on how stdout and stderr will mix on the terminal output.
                io.stdout:write("unknown instruction:\t" .. code[pc] .. " at line:\t" .. tostring(pc) .. "\n")   --lpeg match catches errors I think
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
    return self.stack.top
end
setmetatable(Run, {__call = function (self, code, mem, run)
    run = Run:new(code, mem, run)
    return run:run()
end})
--------------------------------------------------------------------------------

return Run