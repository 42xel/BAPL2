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

V = setmetatable({}, {
    __call = lpeg.V,
    __index = function (self, key)
        return self(key)
    end,
})
setmetatable(_G, getmetatable(V))   --what could possibly go wrong ?