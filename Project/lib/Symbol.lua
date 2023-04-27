--------------------------------------------------------------------------------
--returns a guaranteed unique key.
--call to create a new unique local key
--index to access an existing global key or create one if needs be
local Symbol = setmetatable({}, {
    __call = function(self, key)
        return {label = key}
    end,
    __index = function(self, key)
        self[key] = self(key)
        return self[key]
    end
})

return Symbol