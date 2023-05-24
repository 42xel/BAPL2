--[[
Symbols are fresh values, generally to be used as keys. They are an elaborate way of doing 
```mySymbole={}```
with the added benaefit of
- (optionnally) using a simily global table of symbols (which doesn't pollute the actual globl environement),
- having a label and a __tostring metamethod.
]]
---@class Symbol
---@field label any

local metaSymbol = {
    __name = "Symbol",
    --[[depending on what it used for key, 'k' is important.
    
'v' is a pretense of efficiency : rather than declaring mySymbole={} on a large enough scope and holding to it, 
we have it allocated only when necessary and forgotten otherwise.
It's a pretense because the gain is negligible compared to even the size of the key on the function call in the code.
    ]]
    __mode = 'kv',
}

function metaSymbol:__call (key)
    return setmetatable({label = key}, getmetatable(self))
end
function metaSymbol:__index (key)
    self[key] = self(key)
    return self[key]
end
function metaSymbol:__tostring ()
    return metaSymbol.__name .. ":\t" .. tostring(self.label)
end


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