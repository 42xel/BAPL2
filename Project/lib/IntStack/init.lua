---@alias IntStackElement number|table
--local pt = require "pt".pt
local Proxy = require "Proxy"

--[[
A prototype for the interpreter Stack and its quirks
]]
---@class IntStack : Proxy
---@field top IntStackElement
local IntStack = {__name = "IntStack"}
IntStack.sizes = setmetatable({}, {__mode = 'k'})
function IntStack:new(t)
    t = getmetatable(self).new(self, t or {})
    self.sizes[t] = math.max(1, rawlen(t))
    return setmetatable(t, self)
end

function IntStack:push(...)   --pushes one or several values and returns self, for chaining
    local n, s = select('#', ...), #self
    self.len = #self + n
    table.move({...}, 1, n, 1 + s, self)
    return self
end
function IntStack:write(v)  --write a single value at the top of the Stack and returns the stack.
    self.top = v
    return self
end
function IntStack:rawPeek(n)
    return table.move(self, 1-n, 0, 1)  --a little flashy aren't we?
end
function IntStack:pop(n)    --pops n values and returns them
    n =  n or 1
    --assert(n <= #self, "can't peek that many items from the stack")
    local r = self:rawPeek(n)
    self.len = #self - n
    return table.unpack(r)
end
--@remark ugly code repetition but there is no graceful way to avoid it.
function IntStack:peek(n)    --peeks at the n top values returns them
    --assert(n <= #self, "can't peek that many items from the stack")
    return table.unpack(self:rawPeek(n or 1))
end
function IntStack:__len()
    return getmetatable(self).sizes[self]
end
IntStack.__shl = IntStack.push
IntStack.__shr = IntStack.write
function IntStack:__tostring()
    local r = IntStack.__name .. ": {\n"
    for i = math.max(#self, rawlen(self)), 1, -1 do
        r = r .. (i == #self and "->" or "") .. "\t" .. tostring (self[i]) .. "\n"
    end
    return r .. "}"
end

function IntStack:_defaultGettersFactory()
    local function __index (stack, k)
        k = k + #stack
        assert(0 < k and k <= #stack, ("IntStack index(%s) out of bound (%s)"):format(k, #stack))
        return rawget(stack, k)
    end
    self._defaultGetters = setmetatable({
        --inherited methods
        get = self,
        set = self,
        --methods
        push = self,
        rawPeek = self,
        pop = self,
        peek = self,
        write = self,
        --proxy fields
        len = function (stack) return #stack end,
        top = function (stack) return rawget(stack, #stack) end,
    }, {__index = function (_, k)
        if type(k) == 'number' then return __index else
            error(("IntStack index %s invalid"):format(k), 2) end
    end})
    return self._defaultGetters
end
function IntStack:_defaultSettersFactory()
    local __index = function (stack, k, v)
        if k <= 0 then k = k + #stack end
        if k < 0 or #stack < k then
            error(("IntStack index(%s) out of bound (%s)"):format(k, #stack), 5)
        end
        rawset(stack, k, v)
    end
    self._defaultSetters = setmetatable({
        --proxy fields
        len = function (stack, _len, v) self.sizes[stack] = v end,
        top = function (stack, _top, v) rawset(stack, #stack, v) end,
    }, {__index = function (_, k)
        if type(k) == 'number' then return __index else
            error(("IntStack index %s invalid"):format(k), 2) end
    end})
    return self._defaultSetters
end

Proxy:new(IntStack)
IntStack.__call = nil

---@diagnostic disable-next-line: cast-type-mismatch
---@cast IntStack ({new: fun(self:Proxy, t:table):IntStack}) | fun(t:table):IntStack
---@diagnostic disable-next-line: param-type-mismatch
return IntStack