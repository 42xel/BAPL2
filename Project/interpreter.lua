local utils = require "utils"

--------------------------------------------------------------------------------
--TODO use a table for switch case
local function run(code, mem, stack)
    stack = stack or Stack{}
    mem = mem or {}
    local pc = 1
    local trace
    local function push(...)
        trace:push('<- ' .. table.concat{...})
        stack:push(...)
    end
    local function pop()
        local value = stack:pop()
        trace:push('-> ' .. value)
        return value
    end

    while true do
        trace = Stack{"instruction: " .. code[pc]}
        if false then
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
            error("unknown instruction")-- : " .. code[pc])
        end
        print(trace:unpack())
        pc = pc + 1
    end
    return stack(0)
end

--------------------------------------------------------------------------------
return run