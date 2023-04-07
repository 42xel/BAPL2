
local lpeg = require "lpeg"
local utils = require "utils"

local _Gmeta = utils.set_GlpegShortHands""
--------------------------------------------------------------------------------
--TODO use a table for switch case
local function run(code, mem, stack)
    stack = stack or Stack{}
    mem = mem or {}
    local pc = 0
    local trace = Stack{}
    local function push(v)
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
--TODO après prototype, utiliser Switch ?
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
        add = popop * function(b, a) return a + b end / push,
        sub = popop * function(b, a) return a - b end / push,
        mul = popop * function(b, a) return a * b end / push,
        div = popop * function(b, a) return a / b end / push,
        mod = popop * function(b, a) return a % b end / push,
        pow = popop * function(b, a) return a ^ b end / push,
        --unary operations
        plus = P'' / push,
        minus = P'' * pop * function(a) return -a end / push,
        --binary operations
        lt = popop * function(b, a) return a < b end / push,
        le = popop * function(b, a) return a <= b end / push,
        gt = popop * function(b, a) return a > b end / push,
        ge = popop * function(b, a) return a >= b end / push,
        eq = popop * function(b, a) return a == b end / push,
        neq = popop * function(b, a) return a ~= b end / push,
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

        --[[
        if false then   --to only have elseif
        elseif code[pc] == "push" then
            pc = pc + 1
            push(code[pc])
        elseif code[pc] == "load" then
            pc = pc + 1
            local id = code[pc]
            push(mem[id])
        elseif code[pc] == "store" then
            pc = pc + 1
            local id = code[pc]
            mem[id] = pop()
            -- push(mem[id]) to return a value, for example a=b=c
        elseif code[pc] == "print" then
            print('@', pop())
        elseif code[pc] == "ret" then
            assert (#stack == 1, "Mauvaise hauteur de stack en fin de return")
            break
--binary operations
        elseif code[pc] == "add" then
            local top = pop()
            push(pop() + top)
        elseif code[pc] == "sub" then
            local top = pop()
            push(pop() - top)
        elseif code[pc] == "mul" then
            local top = pop()
            push(pop() * top)
        elseif code[pc] == "div" then
            local top = pop()
            push(pop() / top)
        elseif code[pc] == "mod" then
            local top = pop()
            push(pop() % top)
        elseif code[pc] == "pow" then
            local top = pop()
            push(pop() ^ top)
--unary operations
        elseif code[pc] == "plus" then
        elseif code[pc] == "minus" then
            push(-pop())
--binary comparision
        elseif code[pc] == "lt" then
            local top = pop()
            push(pop() < top and 1 or 0)
        elseif code[pc] == "gt" then
            local top = pop()
            push(pop() > top and 1 or 0)
        elseif code[pc] == "ge" then
            local top = pop()
            push(pop() >= top and 1 or 0)
        elseif code[pc] == "le" then
            local top = pop()
            push(pop() <= top and 1 or 0)
        elseif code[pc] == "eq" then
            local top = pop()
            push(pop() == top and 1 or 0)
        elseif code[pc] == "neq" then
            local top = pop()
            push(pop() ~= top and 1 or 0)
        else
            print(trace:unpack())
            --should not be happening, if it does, there most likely is an error in the compiler.
            error("unknown instruction:\t" .. code[pc] .. " at line:\t" .. tostring(pc))-- : " .. code[pc])
        end
        print(trace:unpack())
        pc = pc + 1
        --]]
        --print (pc, code[pc])
    until runSwitch[code[pc]]:match''
    return stack:unpack()
end

--------------------------------------------------------------------------------
setmetatable(_G, _Gmeta)

return run