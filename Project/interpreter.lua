---@TODO : rewrite as a register machine in C++, Kotlin or Rust

local pt = require"pt".pt
local lpeg = require "lpeg"
local Object = require "Object"
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

local function checkIndexRange(a, i)
    assert(i>0 and i <= #a, ("Array index out of range. Array : %s of size %s, trying index %s"):format(a, a.size, i))
end
local function concat(sep, ...)
    return table.concat(table.move({...}, 1, select('#', ...), 1,
        setmetatable({}, {__newindex = function (self, k, v)
            rawset(self, k, pt(v):gsub('\n', ' '))
        end})), sep)
end

local Array = {__name = 'Array'}
do  --localising ArraySizes
    local ArraySizes = setmetatable({}, {__weak='k'})   ---we're only storing information around arrays here, we don't want to keep them alive.
    function Array:new (size)
        assert(type(size) == 'number')
        local r = setmetatable({size = size or 0}, self)
        ArraySizes[r] = size
        return r
    end
    function Array:__len ()
        return ArraySizes[self]
    end
end
function Array:__index(k)   --we won't do any inheritance with Array, so we don't need to put self.__index ) self in Array.new
    assert(type(k) == 'number')
    return '_'
end
do  --localising depth
    local depth = 0
    function Array:__tostring()
        local r = "\n" .. string.rep("\t", depth) .. "[ " --making room before
        depth = depth + 1

        local ts = {}
        local d = depth
        for i = 1, #self do
            table.insert(ts, tostring(self[i]))
        end
        assert(d == depth)
        r = r .. table.concat(ts, ", ")
        depth = depth - 1
        r = r .. " ]" .."\n" .. string.rep("\t", depth) --making room after

        return r:gsub("%]\n(\t*), \n(\t*)%[", "], \n%1[") --removing double line break from consecutive arrays
                :gsub("%]\n(\t*), ","],\n%1")   --putting commas at the end of lines rather than the beginning
                :gsub("\n\t(\t*) %]", "\n%1]")  --aligning lone closing brackets with their partner.
    end
end
setmetatable(Array, {__call = Array.new})

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
        if type(...) ~= 'number' and type(...) ~= 'table' then error(("trying to push a value which is neither a number nor an Array:\t%s of type:\t%s"):format(..., type(...)) ) end ---@TODO : make it only number/pointers when rewriting the VM.
        trace:push('<- ' .. concat(', ', ...))
        stack:push(...)
    end
    local function pop(_, _, n, ...) n = tonumber(n)   --no capture => captures whole match, here ''. so let us tranform it back to nil.
        --shouldn't happen, if it does, it's most likely an error in the compiler.
        n = n or 1 assert(n <= #stack, ([[trying to pop the stack while empty!
At opCode line %s
Trying to pop %s values
from stack %s]]):format(tonumber(pc), n, pt(stack)))
        trace:push('-> ' .. concat(', ', stack:unpack(#stack - n + 1)))
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

        ---for now, arrays are simply lua tables
        ---@TODO ponder a while when reimplementing the vm in a lower level language as a registr machine
        ---@TODO ponder 0 or 1 index
        new = P'' * pop / function (size) return Array(size) end / push,
        set = Cmt(Cc(3), pop) / function (a, i, v) checkIndexRange(a, i); a[i] = v end,
        get = Cmt(Cc(2), pop) / function (a, i) checkIndexRange(a, i); return assert(a[i], ("No value for table %s at index %s"):format(a, i)) end / 1 / push,    ---@TODO think of default value. is it nil, is it garbage ? do we keep it as an error ?
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