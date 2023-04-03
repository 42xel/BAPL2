local utils = require "utils"

--------------------------------------------------------------------------------
local function run(code, stack)
    stack = stack or Stack{}
    local pc = 1
    local trace
    local function push( ...)
        trace:push('<- ' .. table.concat{...})
        stack:push(...)
    end
    local function pop()
        local value = stack:pop()
        trace:push('-> ' .. value)
        return value
    end
    while pc <= #code do
        trace = Stack{"instruction: " .. code[pc]}
        if code[pc] == "push" then
            pc = pc + 1
            push(code[pc])
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
        elseif code[pc] == "plus" then
        elseif code[pc] == "minus" then
            push(-pop())

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
    return stack[1]
end

--------------------------------------------------------------------------------
return run