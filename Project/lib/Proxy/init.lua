--[[
    Proxies are common place in prototypal OO.
    However, they're more of a design patern than a precise conept I could generically implement.
    Thus the present file is not intended to be a functioning module, but rather a test laboratory related to proxying.
]]
local _README = _README
---@alias ProxyGettersTable {[any] : table|fun(self:table,key:any):value:any}
---@alias ProxySettersTable {[any] : table|fun(self:table,key:any,value:any)}

---@class Proxy : table
---@field _defaultGetters? ProxyGettersTable
---@field _defaultGettersFactory? fun(self:Proxy) : ProxyGettersTable
---@field _defaultSetters? ProxySettersTable
---@field _defaultSettersFactory? fun(self:Proxy) : ProxySettersTable
---@field metaGetters? {[Proxy]: ProxyGettersTable}
---@field metaSetters? {[Proxy]: ProxySettersTable}
local Proxy = {__name = "Proxy"}
--[[
    A Proxy constructor.
    
    A proxy behaves as if they had certain fields and properties without actually holding them.
    It allows many things among which privacy, sand-boxing, as well as custom setters and getters.
    ]]
    ---@param destination Proxy the resulting proxy.
    ---@param getters? ProxyGettersTable a table of getters. <br>Each entry behaves like an `__index` metamethod of `destination` for a specific key. In other words, `destination[missingKey]` yields, if `getters[missingKey]` is a function, `getters[missingKey](destination, missingKey)`, otherwise `getters[missingKey][missingKey]`. Great fun ensues when getters itself has an `__index` metamethod. <br> If not provided, it returns a table
    ---@param setters? ProxySettersTable a table of setters. <br>Each entry behaves like an `__newindex` metamethod of `destination` for a specific key. In other words, `destination[newKey] = value` invokes, if `setters[newKey]` is a function, `setters[newKey](destination, newKey, value)`, otherwise `setters[newKey][newKey] = value`. Great fun ensues when setters itself has an `__index` metamethod.
    ----@return destination, getters, setters
function Proxy:new(destination, getters, setters)
    self:init()
    destination = destination or {}
    getters = getters or self._defaultGetters
        or self._defaultGettersFactory and self:_defaultGettersFactory()
        or {}
    self.metaGetters[destination] = getters
    setters = setters or self._defaultSetters
        or self._defaultSettersFactory and self:_defaultSettersFactory()
        or {}
    self.metaSetters[destination] = setters
    return setmetatable(destination, self), getters, setters
end
local __mode = {__mode = 'k'}
Proxy.init = setmetatable({}, {__call = function (init, proto)

    if init[proto] then return false end
    if proto.metaGetters == nil then proto.metaGetters = setmetatable({}, __mode) end
    if proto.metaSetters == nil then proto.metaSetters = setmetatable({}, __mode) end

    if rawget(proto, '__index') == nil then
        rawset(proto, '__index',
        ---@param self table the proxy missing the key `k`.
        function (self, k)
            local index = proto.metaGetters[self][k] or rawget
            if type(index) == 'function' then
                return index(self, k)
            else
                return index[k]
            end
        end)
    end
    if rawget(proto, '__newindex') == nil then
        rawset(proto, '__newindex',
        ---@param self table the proxy missing the key `k`.
        function (self, k, v)
            local index = proto.metaSetters[self][k] or rawset
            if type(index) == 'function' then
                index(self, k, v)
            else
                index[k] = v
            end
        end)
    end
    if rawget(proto, '__call') == nil then
        rawset(proto, '__call', proto.new) end
    init[proto] = true
    return true
end})

function Proxy:_defaultGettersFactory()
    return {init = self, new = self, get = self, set = self, target = self,
        getters = function (instance) return self.metaGetters[instance] end,
        setters = function (instance) return self.metaSetters[instance] end,
        _defaultGettersFactory = self,
        _defaultSettersFactory = self,
    }
end

---gets the value according to the getter first (before rawget).
function Proxy:get(k)
    return getmetatable(self).__index(self, k)
end
---sets the value according to the setter first (before rawset).
function Proxy:set(k, v)
    getmetatable(self).__newindex(self, k, v)
end

--adds a table to the pseudo index of a proxy
---@param target table
---@param mode? 'i'|'n'|'in' target index, newindex, or both.
function Proxy:target(target, mode)
    local proto = getmetatable(self)
    if mode == nil or 'in' then
        for key, _ in pairs(target) do
            proto.metaGetters[self][key] = target
            proto.metaSetters[self][key] = target
        end
    elseif mode == 'i' then
        for key, _ in pairs(target) do
            proto.metaGetters[self][key] = target
        end
    elseif mode == 'n' then
        for key, _ in pairs(target) do
            proto.metaSetters[self][key] = target
        end
    else error("wrong mode for Proxy:target\texpected 'i'|'n'|'in', got " .. tostring(mode)) end
end

---@diagnostic disable-next-line: cast-type-mismatch
---@cast Proxy (fun(destination?: Proxy|table, getters?:ProxyGettersTable, setters?: ProxySettersTable): Proxy, table, table) | Proxy
---@diagnostic disable-next-line: param-type-mismatch
return setmetatable(Proxy, {__call = Proxy.new})