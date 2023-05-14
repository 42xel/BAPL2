---@class Symbol
---@field label any

local metaSymbol = {
    __name = "Symbol",
    __call = function(self, key)
        return setmetatable({label = key}, getmetatable(self))
    end,
    __index = function(self, key)
        self[key] = self(key)
        return self[key]
    end,
    --[[depending on what it used for key, 'k' is important.
    
'v' is a pretense of efficiency : rather than declaring mySymbole={} on a large enough scope and holding to it, 
we can not only declare it locally, we have it allocated only when necessary.
It's a pretense because the gain is negligible compared to even the size of the key on the function call in the code.
    ]]
    __mode = 'kv',
}

--------------------------------------------------------------------------------
--returns a guaranteed unique key.
--call to create a new unique local key
--index to access an existing global key or create one if needs be
---
--it is essentially the same as using mySymbole={}, but less efficient and more readible.
local Symbol = setmetatable({}, metaSymbol)

---@diagnostic disable-next-line: cast-type-mismatch
---@cast Symbol (fun(key:any):Symbol) | {[any]:Symbol}
return Symbol