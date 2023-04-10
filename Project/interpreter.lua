local pt = require"pt".pt
local lpeg = require "lpeg"
local utils = require "utils"

local _Gmeta = utils.set_GlpegShortHands""
--TODO : use a register machine for more expressiveness, notably giving the option to implement your left/right/logical-value idea on the dynamic level.
--------------------------------------------------------------------------------
--TODO use a table for switch case
local function run(code, mem, stack)
    stack = stack or Stack{}
    mem = mem or {}
    local pc = 0
    local trace = Stack{}
    local function push(v)
        --shouldn't happen, if it does, it most likely is an error in the compiler
        --print()
        --print(pt(code))
        --print(pt(mem))
        --print(pt(stack))
        if type(v) ~= "number" then error("trying to push a non number value:\t" .. tostring(v) ) end
        trace:push('<- ' .. v)
        stack:push(v)
    end
    local function pop()
        local value = stack:pop()
        --shoulldn't happen, if it does, it's most likely an error in the compiler.
        trace:push('-> ' .. assert(value, "trying to pop the stack while empty! at opCode line " .. tonumber(pc)))
        return true, value
    end
    local function inc()
        pc = pc + 1
        return true
    end
    local function line()
        return true, code[pc]
    end

    local popop = P'' * pop * pop
    local function boolToInt (a) return a ~= 0 and 1 or 0 end
--TODO after prototype/proxy is done, use Switch ? The issue here being
    local runSwitch = { -- = lpeg.Switch {
        push = P'' * inc * line / push,
        load = Cc(mem) * inc * line / get / push,
        store = Cc(mem) * inc * line * pop / set,
        print = Cc'@' * pop / print,
        ret = Cc"Mauvaise hauteur de stack en fin de return" / function (err)
            assert(#stack == 1, err)
            return true
        end,
        --binary operators
        add = popop / function(b, a) return a + b end / push,
        sub = popop / function(b, a) return a - b end / push,
        mul = popop / function(b, a) return a * b end / push,
        div = popop / function(b, a) return a / b end / push,
        mod = popop / function(b, a) return a % b end / push,
        pow = popop / function(b, a) return a ^ b end / push,
        --unary operations
        plus = P'' / push,
        minus = P'' * pop / function(a) return -a end / push,
        ["not"] = P'' * pop / function(a) return a == 0 and 1 or 0 end / push,
        --binary operations
        lt = popop / function(b, a) return a < b end / boolToInt / push,
        le = popop / function(b, a) return a <= b end / boolToInt / push,
        gt = popop / function(b, a) return a > b end / boolToInt / push,
        ge = popop / function(b, a) return a >= b end / boolToInt / push,
        eq = popop / function(b, a) return a == b end / boolToInt / push,
        neq = popop / function(b, a) return a ~= b end / boolToInt / push,
        [lpeg.Switch.default] = C"unknown instruction:\t" / function (err)
            print(trace:unpack())
            --should not be happening, if it does, there most likely is an error in the compiler.
            error("unknown instruction:\t" .. code[pc] .. " at line:\t" .. tostring(pc))
        end
    }

    repeat
        inc()
        print(trace:unpack())
        trace = Stack{tostring(pc) .. "\tinstruction: " .. code[pc]}
    until runSwitch[code[pc]]:match'' == true
    return stack:unpack()
end

--------------------------------------------------------------------------------
setmetatable(_G, _Gmeta)

return run