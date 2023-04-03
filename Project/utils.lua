lpeg = require"lpeg"
Stack = {
    push = function(self, ...)
        for _,v in ipairs {...} do
            table.insert(self, v)
        end
    end,
    pop = table.remove,
    unpack = table.unpack,
    __call = function(self, n)
        return type(n) == "number" and (n > 0 and self[n] or self[#self + n])
    end,
}
Stack.__index = Stack
setmetatable(Stack, {
    __call = function(self, t) return setmetatable(t, self) end,
})

V = {
    __call =function (_, ...)
        return lpeg.V(...)
    end,
    __index = function (self, key)
        self[key] = self(key)
        return self[key]
    end,
}
setmetatable(V, V)
setmetatable(_G, V)   --what could possibly go wrong ?