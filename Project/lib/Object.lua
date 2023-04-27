--------------------------------------------------------------------------------
--defining __call as a default constructor.
---@class Object
local Object = {__name = "Object"}
function Object:new(t)  ---@return Object
    self.__index = rawget(self, "__index") or self
    self.__call = rawget(self, "__call") or self.new
    t = setmetatable(t or {}, self)
    return t
end


return Object.new({__index = {}, new = Object.new}, Object)
