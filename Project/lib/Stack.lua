local Object = require"Object"

--------------------------------------------------------------------------------
--TODO refactor in the light of OOP (textbook)
--TODO description
---@class Stack : Object
local Stack = Object:new{}
function Stack:push (...)
    for _,v in ipairs {...} do
        table.insert(self, v)
    end
end
function Stack:pop (n) --pops one or several, in preserved order (inverse order of popping)
    if n == nil or n == 1 then return table.remove(self, n) end
    local r = {}
    if n <= 0 then n = n + #self end    --0 : pops all, -1 pops all but 1, etc.
    for i = n, 1, -1 do
        r[i] = self:pop()
    end
    return table.unpack(r)
end
Stack.unpack = table.unpack
--a destructive method which returns a sequence of elements in order of popping (reverse order of indices)
function Stack:_unstack_aux (n, ...)
    if #self > 0 and n > 0 then
        return self:_unstack_aux(n - 1, self:pop(), ...)
    else return ... end
end
function Stack:unstack (n)
    return self:_unstack_aux(n or #self)
end
-- s(-1) gets the last element, s(-2) the next to last, etc.
function Stack:__call (n)
    return type(n) == "number" and (n > 0 and self[n] or self[#self + n + 1])
end

return Stack