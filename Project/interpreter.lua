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

local function checkIndexRange(a, i)
    assert(type(a) == 'table', ("Trying to index something other than a table:\t%s index:\t%s"):format(a, i))
    assert(i>0 and i <= #a, ("Array index out of range. Array : %s of size %s, trying index %s"):format(a, a.size, i))
end
---@TODO on the lua discussion boards, see why concat doesn't use tostring and __tostring and argue/code in favor of it.
local function concat(t, sep, i, j)
    sep = sep or ', '
    i = i or 1
    j = j or #t
    return table.concat(table.move(t, i, j, 1,
        setmetatable({}, {__newindex = function (self, k, v)
            rawset(self, k, tostring(v):gsub('[\n\t]', ' '))
        end})), sep)
end

---@param code Stack
---@param mem? table
---@param stack? IntStack
local function run(code, mem, stack)
    stack = stack or IntStack{}
    mem = mem or {}
    local pc = 0
    local trace = Stack{}
    local function push(...)
        --shouldn't happen, if it does, it most likely is an error in the compiler
        if type(...) ~= 'number' and type(...) ~= 'table' then error(("trying to push a value which is neither a number nor an Array:\t%s of type:\t%s"):format(..., type(...)) ) end ---@TODO : make it only number/pointers when rewriting the VM.
        trace:push('<- ' .. concat({...}))
        --trace:push(tostring(stack):gsub('[\n\t]', ' '))
        stack:push(...)
    end
    local function write (v)
        stack:write(v)
    end
    local function pop(_, _, n) n = tonumber(n)   --no capture => captures whole match, here ''. so let us tranform it back to nil.
        --shouldn't happen, if it does, it's most likely an error in the compiler.
        n = n or 1 assert(n <= #stack, ([[trying to pop the stack while empty!
At opCode line %s
Trying to pop %s values
from stack %s]]):format(tonumber(pc), n, pt(stack)))
        trace:push('-> ' .. concat(stack, nil, #stack - n + 1))
        --trace:push(pt(stack):gsub('[\n\t]', ' ') .. '')
        return true, stack:pop(n)
    end
    local function peek(_, _, n) n = tonumber(n)
        --shouldn't happen, if it does, it's most likely an error in the compiler.
        n = n or 1 assert(n <= #stack, ([[trying to pop the stack while empty!
At opCode line %s
Trying to pop %s values
from stack %s]]):format(tonumber(pc), n, pt(stack)))
        return true, stack:peek(n)
    end
    local function inc()
        pc = pc + 1
        return true
    end
    local function line()
        return true, code[pc]
    end
    local function boolToInt (a) return a and 1 or 0 end

---@TODO after prototype/proxy is done, use Switch ? The issue here being
---@TODO use meta programming to avoid code repetition ?
---I don't know, with Ultra editing, contiguous one liner code repetition is painless and harmless, and meta programming doesn't help when using lua-language-server
    local runSwitch = lpeg.Switch {
        --basic
        push = P'' * inc * line / push,
        write = P'' * inc * line / write,
        pop = P'' * pop / 0,
        dup = P'' * peek / push,
        load = Cc(mem) * inc * line / get / push,
        store = Cc(mem) * inc * line * pop / set,
        print = Cc'@' * pop / print,
        ret = Cc"Incorrect Stack height upon return:\n" / function (err)
            assert(#stack == 1, err .. tostring(stack) .. "\t len:\t" .. #stack)
            return true
        end,
        --control structures
        jmp            = P'' * inc * line / function (d)                   pc = pc + d end,
        jmp_Z   = P'' * peek * inc * line / function (a, d) if a == 0 then pc = pc + d end end,
        jmpop_Z  = P'' * pop * inc * line / function (a, d) if a == 0 then pc = pc + d end end,
        jmp_NZ  = P'' * peek * inc * line / function (a, d) if a ~= 0 then pc = pc + d end end,
        jmpop_NZ = P'' * pop * inc * line / function (a, d) if a ~= 0 then pc = pc + d end end,
        ---@TODO use meta programming ?
        --binary operations
        add0= Cmt(Cc(2), pop) / function(a, b) return a + b end             / push,
        add = P(function() stack.top = stack[-1] + stack:pop() end),
        add2= P(function() stack:write(stack[-1] + stack:pop()) end),
        sub = Cmt(Cc(2), pop) / function(a, b) return a - b end             / push,
        mul = Cmt(Cc(2), pop) / function(a, b) return a * b end             / push,
        div = Cmt(Cc(2), pop) / function(a, b) return a / b end             / push,
        mod = Cmt(Cc(2), pop) / function(a, b) return a % b end             / push,
        pow = Cmt(Cc(2), pop) / function(a, b) return a ^ b end             / push,
    ["and"] = Cmt(Cc(2), pop) / function(a, b) return a == 0 and a or b end / push,
    ["or"]  = Cmt(Cc(2), pop) / function(a, b) return a == 0 and b  or a end/ push,
        --unary operations
        plus = P'' / push,
        minus0 = P'' * peek / function(a) return -a end / write,
        minus  = P(function() stack.top = -stack.top end),
        ["not"] = P'' * pop / function(a) return a == 0 and 1 or 0 end / push,
        --binary comparisons
        lt = Cmt(Cc(2), pop) / function(a, b) return a < b end / boolToInt / push,
        le = Cmt(Cc(2), pop) / function(a, b) return a <= b end / boolToInt / push,
        gt = Cmt(Cc(2), pop) / function(a, b) return a > b end / boolToInt / push,
        ge = Cmt(Cc(2), pop) / function(a, b) return a >= b end / boolToInt / push,
        eq = Cmt(Cc(2), pop) / function(a, b) return a == b end / boolToInt / push,
        neq = Cmt(Cc(2), pop) / function(a, b) return a ~= b end / boolToInt / push,
        --chained comparisons
        c_lt = Cmt(Cc(2), pop) / function(a, b) return b,  boolToInt(a < b) end / push,
        c_le = Cmt(Cc(2), pop) / function(a, b) return b,  boolToInt(a <= b) end / push,
        c_gt = Cmt(Cc(2), pop) / function(a, b) return b,  boolToInt(a > b) end / push,
        c_ge = Cmt(Cc(2), pop) / function(a, b) return b,  boolToInt(a >= b) end / push,
        c_eq = Cmt(Cc(2), pop) / function(a, b) return b,  boolToInt(a == b) end / push,
        c_neq = Cmt(Cc(2), pop) / function(a, b) return b,  boolToInt(a ~= b) end / push,
        --arrays

        ---for now, arrays are simply lua tables
        ---@TODO ponder a while when reimplementing the vm in a lower level language as a registr machine
        ---@TODO ponder 0 or 1 index
        new = P'' * pop / function (size) return Array(size) end / push,
        c_new = P'' * pop / function (size) return Array(size), size end / push,
        set = Cmt(Cc(3), pop) / function (a, i, v) checkIndexRange(a, i); a[i] = v end,
        c_set = Cmt(Cc(3), pop) / function (a, i, v) checkIndexRange(a, i); a[i] = v return a, i end / push,
        get = Cmt(Cc(2), pop) / function (a, i) checkIndexRange(a, i); return assert(rawget(a, i), ("No value for table %s at index %s"):format(a, i)) end / 1 / push,    ---@TODO think of default value. is it nil, is it garbage ? do we keep it as an error ?
        [lpeg.Switch.default] = Cc"unknown instruction:\t" / function (err)
            print(trace:unpack())
            --should not be happening, if it does, there most likely is an error in the compiler.
            ---@TODO output in stderr. for now I use stdout for readibility, because apparently I have no control on how stdout and stderr will mix on the terminal output.
            io.stdout:write(err .. code[pc] .. " at line:\t" .. tostring(pc) .. "\n")   --lpeg match catches errors I think
            os.exit(false)
        end
    }
    repeat
        inc()
        print(trace:unpack())
        trace = Stack{tostring(pc) .. "\tinstruction: " .. code[pc]}    --TODO : print trace and program outpout to different streams ?
    until runSwitch(code[pc]) == true -- or print(pc, code[pc], runSwitch[code[pc]])
    return stack:peek(#stack)
end

--------------------------------------------------------------------------------

return run