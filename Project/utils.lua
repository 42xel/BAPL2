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

local debogue = {
    trace = function(...)
        if _DEBOGUE then
            print(...)
        end
    end,
    MaxOffset = 0,
    I = function (tag)
        return lpeg.P(function ()
            print(tag)
            return true
        end)
    end,
}
debogue.ws_suffix = function (_, p)
    debogue.MaxOffset = math.max(debogue.MaxOffset, p)
    return true
end

--TODO : switch case of sort, with RPNfold of arguments

--------------------------------------------------------------------------------
return {
    debogue = debogue
}