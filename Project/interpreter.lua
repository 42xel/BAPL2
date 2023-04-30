---@TODO : rewrite as a register machine in C++, Kotlin or Rust

local pt = require"pt".pt
local lpeg = require "lpeg"
local Stack = require"Stack"
require "utils"

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
---@param code Stack
---@param mem? table
---@param stack? Stack
local function run(code, mem, stack)
    stack = stack or Stack{}
    mem = mem or {}
    local pc = 0
    local trace = Stack{}
    local function push(...)
        --shouldn't happen, if it does, it most likely is an error in the compiler
        if type(...) ~= "number" then error("trying to push a non number value:\t" .. tostring(...) ) end
        trace:push('<- ' .. table.concat({...}, ","))
        stack:push(...)
    end
    local function pop(_, _, n, ...) n = tonumber(n)   --no capture => captures whole match, here ''. so let us tranform it back to nil.
        --shouldn't happen, if it does, it's most likely an error in the compiler.
        n = n or 1 assert(n <= #stack, ([[trying to pop the stack while empty!
At opCode line %s
Trying to pop %s values
from stack %s]]):format(tonumber(pc), n, pt(stack)))
        trace:push('-> ' .. table.concat({stack:unpack(#stack - n + 1)}, ','))
        return true, stack:pop(n)
    end
    local function peek()
        return true, stack:peek()
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
        pop = P'' * pop / 0,
        load = Cc(mem) * inc * line / get / push,
        store = Cc(mem) * inc * line * pop / set,
        print = Cc'@' * pop / print,
        ret = Cc"Mauvaise hauteur de stack en fin de return:\n" / function (err)
            assert(#stack == 1, err .. pt(stack))
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
        add = Cmt(Cc(2), pop) / function(a, b) return a + b end             / push,
        sub = Cmt(Cc(2), pop) / function(a, b) return a - b end             / push,
        mul = Cmt(Cc(2), pop) / function(a, b) return a * b end             / push,
        div = Cmt(Cc(2), pop) / function(a, b) return a / b end             / push,
        mod = Cmt(Cc(2), pop) / function(a, b) return a % b end             / push,
        pow = Cmt(Cc(2), pop) / function(a, b) return a ^ b end             / push,
    ["and"] = Cmt(Cc(2), pop) / function(a, b) return a == 0 and a or b end / push,
    ["or"]  = Cmt(Cc(2), pop) / function(a, b) return a == 0 and b  or a end/ push,
        --unary operations
        plus = P'' / push,
        minus = P'' * pop / function(a) return -a end / push,
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
        new = P'',
        set = P'',
        get = P'',
        [lpeg.Switch.default] = C"unknown instruction:\t" / function (err)
            print(trace:unpack())
            --should not be happening, if it does, there most likely is an error in the compiler.
            error("unknown instruction:\t" .. code[pc] .. " at line:\t" .. tostring(pc))
        end
    }
    repeat
        inc()
        print(trace:unpack())
        trace = Stack{tostring(pc) .. "\tinstruction: " .. code[pc]}    --TODO : print trace and program outpout to different streams ?
    until runSwitch(code[pc]) == true -- or print(pc, code[pc], runSwitch[code[pc]])
    return stack:unpack()
end

--------------------------------------------------------------------------------

return run