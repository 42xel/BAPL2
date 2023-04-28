
--a basic object enabling inheritance
--@class Prototype
local Prototype = {__name = "Prototype"}
function Prototype:new(t)
    t = t or {}
    self.__index = self
    return setmetatable(t, self)
end

function ProxyGen(getter, setter) return function (target, remote)
    getter = getter or function (target) return function (self, k)
        return target[k]
    end end
    getter = setter or function (target) return function (self, k, v)
        target[k] = v
    end end
    return setmetatable(remote or {}, {__index = getter(target), __newindex = setter(target)})
end end

--[[
--creates and maintain a dual object
Dual = Object{
    proxy = ProxyGen(nil,
        -- setter
        function (target) return function (self, k, v)
            assert(k ~= nil) --should error before that point but hey
            if v == nil then --erasure
                v = target[k]
                if v ~= nil then --else we've already cleaned, beacause we entered from self's dual
                    target[k] = nil
                    self.dual[v] = nil
                end
                return
            end

            if target[k] ~= nil then
                self[k] = nil
            end

            target[k] = v
            if self.dual[v] == nil then --if not already set
                self.dual[v] = k
            end
        end end
    )
}
function Dual:new(t1, t2)
    t1 = t1 or {}
    t2 = t2 or {}
--TODO fill t1 and t2
    local t3 = {}

    for k, v in pairs(t1) do
        assert(t3[v] == nil, "t1 not injective")
        t3[v] = k
    end
    for k, v in pairs(t2) do
        assert(t1[v] == nil or t1[v] == k)
        t1[v] = k
    end
    for k, v in pairs(t3) do
        assert(t2[k] == nil or t1[k] == v)
        t1[k] = v
    end

    local r1, r2 = {}, {}
    r1.dual = self.proxy(t2, r2)
    r2.dual = self.proxy(t1, r1)
    return r1, r2
end

--]]