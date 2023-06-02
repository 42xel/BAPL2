---@alias ContextElement number|Context|nil
--local pt = require "pt".pt
--local Array = require "Array"
local Proxy = require "Proxy"
local Array = require "Array"

---@TODO Proxy is a bit overkill and makes it harder to implement low level later on, doesn't it ?

--[[
A prototype for the ubiquitous context, which serves as function and block code environment, array, and memory.

To implement using a C++ vector or two, or something similar I guess. Or an array when the size is known.
What's currently metadata should probably be regular data as well, on what language.the VM is eventually rewritten.
]]
---@class Context : Proxy
---@field head ContextElement
---@field parent Context
---@field caller Context
local Context = {__name = "Context",
    validTypes = {
        number = true,
        table = true,
        ["nil"] = true,
    }
}
Context.hpos = 0
Context.memlen = 0
Context.arrlen = 0

---@TODO : clean that thing up
--the toplevel parent
Context.parent = {} -- {write = function (_, v) print(v) end, up = print}

Context.caller = nil
Context.hpos = 1
Context[0] = Context

---@TODO (for memory cleaning, after VM rewrite) add owner ?
---@param t? {arrlen?: number, hpos?: number, memlen?: number}
---@return Proxy
function Context:new(t)
    t = getmetatable(self).new(self, t or {}) --inheritance of sort (here inheriting from Proxy)

    t.arrlen = rawget(t, "arrlen") or rawlen(t)
    t.hpos = rawget(t, "hpos") or 1
    if not t.memlen then
        t.memlen = - 1
        while rawget(t, t.memlen) do
            t.memlen = t.memlen - 1
        end
        t.memlen = - 1 - t.memlen
    end
    t[0] = t
    --t.parent = self   --don't if you don't need I guess ?
    --t.caller = self   --caller ccorresponds to `caller \ callee` ?

    return setmetatable(t, self)
end

function Context:cpy(t)
    --t.arrlen = t.arrlen or self.arrlen
    --t.hpos = t.hpos or self.hpos
    --t.memlen = t.memlen or self.memlen
    --t.parent = t.parent or self.parent
    t = t or {}
    for key, value in pairs(self) do
        t[key] = rawget(t, key) or value
    end
    return Context:new(t)
end

function Context:push(...)   --pushes one or several values to the stack and returns self, for chaining
    local n, s = select('#', ...), self.hpos
    self.hpos = s + n
    table.move({...}, 1, n, 1 + s, self)
    return self
end
function Context:write(v)  --write a single value at the top of the Stack and returns the stack.
    ---@TODO check whether good/necessary or whether it'd be better to error.
    if self.hpos == 0 then
        print("Context:write(): warning head position was zero")
        self.hpos = 1;
    end
    self.head = v
    return self
end
function Context:rawPeek(n)
    return table.move(self, self.hpos + 1 - n, self.hpos, 1, {})
end
function Context:pop(n)    --pops n values and returns them
    n = math.min(self.hpos, n or 1)
    local r = self:rawPeek(n)
    self.hpos = self.hpos - n
    return table.unpack(r)
end
function Context:peek(n)    --peeks at the n top values returns them
    n = math.min(self.hpos, n or 1)
    return table.unpack(self:rawPeek(n))
end
function Context:clean()
    for i = math.max(1, self.hpos), rawlen(self) do
        rawset(self, i, nil)
    end
end
--function Context:pack()
--    local r = {}
--    for i = #self, rawlen(self) do
--        table.insert(r, rawget(self, i))
--        rawset(self, i, nil)
--    end
--    self.top = Array(r)
--end
function Context:__len()
    return math.max(self.hpos, self.arrlen, rawlen(self))
end
--Context.__shl = Context.push
--Context.__shr = Context.write
function Context:__tostring()
    if self.memlen == 0 and self[0] == self then    --litteral Array I guess
        return Array.__tostring(self)
    end
    local r = Context.__name .. ": {\n"
    for i = math.max(self.hpos, rawlen(self), self.arrlen), 1, -1 do
        ---@TODO show variables ? (i < 0)
        r = r .. (i == self.hpos and "->" or "") .. "\t" .. tostring (self[i]) .. "\n"
    end
    return r .. "}"
end

function Context:_defaultGettersFactory()
    local function __index (stack, k)
        if type(k) ~= 'number' then ---@TODO should never happen, remove
            error("Context key, type error:\t" .. type(k) .. " " .. tostring(k))
        end
--        assert(-stack.memlen <= k and k <= stack.arrlen, ("Context index(%s) out of bounds (-%s, %s)"):format(k, stack.memlen, stack.arrlen))
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
        clean = self,
        --pack = self,
        validTypes = self,
        cpy = self,
        --fields
        memlen = rawget, --self,
        arrlen = rawget, --self,
        hpos   = rawget, --self,
        parent = rawget, --self,
        caller = rawget, --self,
        func   = rawget, --self,
        _fun   = rawget, --self, --a flag to know whether the context is a function
        --proxy (pseudo) fields
        head = function (stack) return rawget(stack, stack.hpos) end,
        ---@TODO
        --proxy register fields
        --the size of the last litteral array. Needs to be a stack itself I'm afraid.
    }, {__index = function (_, k)
        if type(k) == 'number' then return __index else
            error(("Array index %s invalid"):format(k), 2) end
    end})
    return self._defaultGetters
end
function Context:_defaultSettersFactory()
    local __index = function (stack, k, v)
        if type(k) ~= 'number' then --should not happen
            error("Array key, type error:\t" .. type(k) .. " " .. tostring(k))
        end
--        if  not(-stack.memlen <= k and k <= stack.arrlen) then
--            ---@TODO separtate error for > 0 (Array) and < 0 (local var of context)
--            error(("Array index(%s) out of bounds (-%s, %s)"):format(k, stack.memlen, stack.arrlen), 5)
--        end
        rawset(stack, k, v)
    end
    self._defaultSetters = setmetatable({
        --fields
        memlen = rawset, -- self,
        arrlen = rawset, -- self,
        hpos   = rawset, -- self,
        parent = rawset, -- self,
        caller = rawset, -- self,
        func   = rawset, --self,
        _fun   = rawset, --self, --a flag to know whether the context is a function
        --proxy fields
        head = function (stack, _head, v) rawset(stack, stack.hpos, v) end,
    }, {__index = function (_, k)
        if type(k) == 'number' then return __index else
            error(("Context index %s invalid"):format(k), 2) end
    end})
    return self._defaultSetters
end

Proxy(Context)
Context()  --initializing by creating an empty Context
Context.__call = nil   --removing an undesired metamethod.
Context.parent = Context:new(Context.parent)
--rawset(Context.parent, 'parent', Context.parent)

---A virtual context, which does not hold information itself but has a parent and a caller pointer to an actual context
---@class VirtualContext : table : Context
---@field parent Context
---@field caller? Context
local VirtualContext = setmetatable({}, {__index = Context})
---comment
---@param t? table|Context
---@param parent Context
---@param caller? Context
---@return any
function VirtualContext:new (t, parent, caller)
    t = t or {}
    t.parent = parent or t.parent
    t.caller = caller or t.caller
    self.__index = self.__index or self
    return setmetatable(t, self)
end
Context.VirtualContext = VirtualContext

return Context
