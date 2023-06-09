---@TODO depreciate. It's more of a design pattern than something to actually derive everything from.
--the whole beauty of lua's prototypal OO is it's freedom.
--------------------------------------------------------------------------------
--defining __call as a default constructor.
---@class Object
local Object = {__name = "Object"}
function Object:new(t)  ---@return Object
    self.__index = rawget(self, "__index") or self
    self.__call = rawget(self, "__call") or self.new --function(self, ...) return self.new(...) end
    t = setmetatable(t or {}, self)
    return t
end

return Object.new({__index = {}, new = Object.new}, Object)