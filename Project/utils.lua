--TODO split utils in utils an lpegUtils
lpeg = require"lpeg"

--------------------------------------------------------------------------------
--a basic object enabling inheritance
Prototype = {}
function Prototype:new(t)
    t = t or {}
    self.__index = self
    return setmetatable(t, self)
end
--defining __call as a default constructor.
Object = Prototype:new{__call = Prototype.new}

function ProxyGen(getter, setter) return function (target, remote)
    getter = getter or function (target) return function (self, k)
        return target[k]
    end end
    getter = setter or function (target) return function (self, k, v)
        target[k] = v
    end end
    return setmetatable(remote or {}, target or {}, {__index = getter(target), __newindex = setter(target)})
end end

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


--------------------------------------------------------------------------------
function get(t, k)
    return t[k]
end
function set(t, k, v)
    t[k] = v
end

--------------------------------------------------------------------------------
--TODO refactor in the light of OOP (textbook)
--TODO description
Stack = Object:new{}
function Stack:push (...)
    for _,v in ipairs {...} do
        table.insert(self, v)
    end
end
function Stack:pop (n) --pops one or several, in preserved order (inverse order of popping)
    if n == nil or n == 1 then return table.remove(self, n) end
    local r = {}
    if n <= 0 then n = n + #self end    --0 : pops all, -1 pops all but 1, etc.
    for i = n, 1, -1 do
        r[i] = self:pop()
    end
    return table.unpack(r)
end
Stack.unpack = table.unpack
--a destructive method which returns a sequence of elements in order of popping (reverse order of indices)
function Stack:_unstack_aux (n, ...)
    if #self > 0 and n > 0 then
        return self:_unstack_aux(n - 1, self:pop(), ...)
    else return ... end
end
function Stack:unstack (n)
    return self:_unstack_aux(n or #self)
end
-- s(-1) gets the last element, s(-2) the next to last, etc.
function Stack:__call (n)
    return type(n) == "number" and (n > 0 and self[n] or self[#self + n + 1])
end

--------------------------------------------------------------------------------
--returns a guaranteed unique key.
--call to create a new unique local key
--index to access an existing global key or create one if needs be
Symbol = setmetatable({}, {
    __call = function(self, key)
        return {label = key}
    end,
    __index = function(self, key)
        self[key] = self(key)
        return self[key]
    end
})
--------------------------------------------------------------------------------
--lpeg fields
--TODO : cleaner, use inheritance or something.

lpeg.Cargs = setmetatable ({
},{
    __call = function(self, i, j)
        if j == nil then
            return self[1][i]
        else
            return self[i][j]
--            return lpeg.Cc(i) * self[j] / select      --neat alternative to only store 1 -> i ranges
        end
    end,
    __index = function(self, i)
        self[i] = setmetatable({}, {
            __index = function(self, j)
                if j < i then
                    return lpeg.P''
                else
                    return self[j-1] * lpeg.Carg(j)
                end
            end
        })
        return self[i]
    end,
})
--simple debugging pattern
--TODO msg.format ? allows much more things
function lpeg.I (msg, ncmt)
    --ncmt = true
    return ncmt and lpeg.P(msg) / print or lpeg.P(function (...) print(msg:format(...)); return true end)
end
--Copies a named pattern.
function lpeg.Cpy(target, source)
    return lpeg.Cg(lpeg.Cb(source or '_'), target)
end

--------------------------------------------------------------------------------
--lpeg switch

local default = Symbol["lpeg.Switch.default"]
local missing = lpeg.P''
lpeg.Switch = setmetatable({
    default = default,
    __call = function(self, case, ...)
        return self[case]:match(case,1, ...)
    end,
    __index = function(self, key)
        return self[default] or missing
    end,
},
{__call = function (self, t)
    return setmetatable(t, self)
end,
})
--------------------------------------------------------------------------------
--TODO change name, developp and document
local debogue = {
    trace = function(...)
        if _DEBOGUE then
            print(...)
        end
    end,
--TODO whats best, MaxOffset or LastOffset ? or both ?
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

--------------------------------------------------------------------------------
return {
    debogue = debogue,
    set_GlpegShortHands = set_GlpegShortHands,
}