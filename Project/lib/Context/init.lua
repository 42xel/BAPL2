---@alias ContextElement number|Context|nil
local pt = require "pt".pt
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
---@field parent? Context
---@field caller? Context
local Context = {__name = "Context",
    validTypes = {
        number = true,
        table = true,
        string = true,
        ["nil"] = true,
    }
}

Context.memlen = 0
Context.arrlen = 0
Context.hpos = 1
Context.ihpos = 1

---@TODO : clean that thing up
--the toplevel parent
Context.parent = {} -- {write = function (_, v) io.stderr:write(v) end, up = io.stderr:write}

--Context.caller = nil
Context[0] = Context

---@TODO (for memory cleaning, after VM rewrite) add owner ?
---@param t? {arrlen?: number, hpos?: number, memlen?: number, parent? : Context} --, caller? : Context}
---@return Proxy
function Context:new(t)
    t = getmetatable(self).new(self, t or {}) --inheritance of sort (here inheriting from Proxy)

    t.arrlen = rawget(t, "arrlen") or rawlen(t)
    t.hpos = rawget(t, "hpos") or 1
    t.ihpos = rawget(t, "ihpos") or t.hpos
    if not t.memlen then
        t.memlen = - 1
        while rawget(t, t.memlen) do
            t.memlen = t.memlen - 1
        end
        t.memlen = - 1 - t.memlen
    end
    t[0] = t[0] or t
    --t.parent = self   --don't if you don't need I guess ?
    --t.caller = self   --caller ccorresponds to `caller \ callee` ?

    return setmetatable(t, self)
end

function Context:cpy(t)
    t = t or {}
    for key, value in pairs(self) do
        t[key] = rawget(t, key) or value
    end
    return Context:new(t)
end

--A context proxy, which reads data (indices > 0) from a context (self) but holds different metadata.
--mostly used for functions, whose definition, value and call instances all share the same static body, but may differ in parent, caller, parameter value ...
---@TODO use that to make virtual contexts ?
---@TODO (vm rework) make Context much simpler with a data field which is an array/vector
function Context:pxy(t, getters, setters)
    local Class = Context -- (self.__name:find'Context' == 1) and self.init and self or Context

    local function getters__index (t, k)
        return self[k]
    end
    getters = getters or setmetatable({}, {__index = function (_, k)
        if type(k) == 'number' then
            if k > 0 then
                return getters__index
            else
                return rawget
            end
        else
            error(("Context index %s invalid"):format(k), 2)
        end
    end})
    getters = Class._defaultGettersFactory(Class, getters)

    local function setters__index (t, k, v)
        self[k] = v
    end
    setters = setters or setmetatable({}, {
        __index = function (_, k)
        if type(k) == 'number' then
            if k > 0 then
                return setters__index
            else
                return rawset
            end
        else
            error(("Context index %s invalid"):format(k), 2)
        end
    end})
    setters = Class._defaultSettersFactory(Class, setters)

    local gettersMT, settersMT = getmetatable(getters), getmetatable(setters)
    setmetatable(getters, nil) ; setmetatable(setters, nil)
    t, getters, setters = Proxy.new(Class, t or {}, getters, setters) --inheritance of sort (here inheriting from Proxy)
    setmetatable(getters, gettersMT); setmetatable(setters, settersMT)

    t.arrlen = rawget(t, "arrlen") or rawlen(self)
    t.hpos = rawget(t, "hpos") or self.hpos or 1
    t.ihpos = rawget(t, "ihpos") or t.hpos
    if not t.memlen then
        t.memlen = - 1
        while rawget(t, t.memlen) do
            t.memlen = t.memlen - 1
        end
        t.memlen = - 1 - t.memlen
    end
    t[0] = t[0] or self[0]

    t = setmetatable(t, Class)
    ---@cast t Context
    return t, getters, setters
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
        io.stderr:write("Context:write(): warning head position was zero")
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
    local i = math.max(1, self.hpos)
    while self [i] ~= nil or i < rawlen(self) do
    --for i = math.max(1, self.hpos), rawlen(self) do
        --rawset(self, i, nil)
        self[i] = nil
        i = i + 1
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
    --io.stderr:write('Context.__tostring', self.hpos or 1, rawlen(self), self.arrlen or 0)
    for i = math.max(self.hpos or 1, rawlen(self), self.arrlen or 0), 1, -1 do
        ---@TODO show variables ? (i < 0)
        r = r .. (i == self.hpos and "->" or "") .. "\t" .. tostring (self[i]) .. "\n"
    end
    return r .. "}"
end

function Context:_defaultGettersFactory(t)
    local function __index (stack, k)
        if type(k) ~= 'number' then ---@TODO should never happen, remove
            error("Context key, type error:\t" .. type(k) .. " " .. tostring(k))
        end
--        assert(-stack.memlen <= k and k <= stack.arrlen, ("Context index(%s) out of bounds (-%s, %s)"):format(k, stack.memlen, stack.arrlen))
        return rawget(stack, k)
    end
    local defaultGetters = self._defaultGetters or setmetatable({
        --inherited fields
        __name = self,
        --inherited methods
        get = self,
        set = self,
        pxy = self,
        --init = self,
        ----new = self,
        _defaultGettersFactory = self,
        _defaultSettersFactory = self,
        _defaultGetters = self,
        _defaultSetters = self,

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
        memlen = rawget,
        arrlen = rawget,
        hpos   = rawget,
        ihpos  = rawget,
        stack  = rawget,
        parent = rawget,
        caller = rawget,
        _fun   = rawget, --a flag to know whether the context is a function
        --proxy (pseudo) fields
        head = function (stack) return rawget(stack, stack.hpos) end,
        ---@TODO
        --proxy register fields
        --the size of the last litteral array. Needs to be a stack itself I'm afraid.
    }, {__index = function (_, k)
        if type(k) == 'number' then return __index else
            error(("Array index %s invalid"):format(k), 2) end
    end})

    if not t then
        self._defaultGetters = defaultGetters
        return self._defaultGetters
    else
        for key, value in pairs(defaultGetters) do
            rawset(t, key, rawget(t, key) or value)
        end
        return setmetatable(t, getmetatable(t) or getmetatable(defaultGetters))
    end
end
function Context:_defaultSettersFactory(t)
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
    local defaultSetters = setmetatable({
        --fields
        memlen = rawset,
        arrlen = rawset,
        hpos   = rawset,
        ihpos  = rawset,
        stack  = rawset,
        parent = rawset,
        caller = rawset,
        _fun   = rawset, --a flag to know whether the context is a function
        --proxy fields
        head = function (stack, _head, v) rawset(stack, stack.hpos, v) end,
    }, {__index = function (_, k)
        if type(k) == 'number' then return __index else
            error(("Context index %s invalid"):format(k), 2) end
    end})

    if not t then
        self._defaultSetters = defaultSetters
        return self._defaultSetters
    else
        for key, value in pairs(defaultSetters) do
            rawset(t, key, rawget(t, key) or value)
        end
        return setmetatable(t, getmetatable(t) or getmetatable(defaultSetters))
    end
end

Proxy(Context)
Context()  --initializing by creating an empty Context
Context.__call = nil   --removing an undesired metamethod.
Context.parent = Context:new(Context.parent)
--rawset(Context.parent, 'parent', Context.parent)

---A virtual context, which does not hold information itself but has a parent and a caller pointer to an actual context
---@class VirtualContext : table : Context
---@field stack Context         the stack supposed to be manipulated and written to.
---@field vtcxParent? VirtualContext    the former vctx to resume to after the end of a block or function
---@field parent? Context       for lexical scoping (upvalues of sort)
---@field caller? Context       for dynamical scoping (parameters)
---@field ihpos? number         the stack head position at the begninning and end of call reguarding this context
local VirtualContext = setmetatable({}, {__index = Context})
---comment
---@param t? {vtcxParent? : VirtualContext, stack? : Context, parent? : Context, caller? : Context, ihpos? : number,} | VirtualContext
---@return VirtualContext
function VirtualContext:new (t) --, stack, parent, caller, ihpos)
    t = t or {}
    t.vtcxParent = t.vtcxParent
    t.stack = t.stack or t.vtcxParent and t.vtcxParent.stack
    t.parent = t.parent or t.vtcxParent and t.vtcxParent.parent or t.stack
    t.caller = t.caller or t.vtcxParent and t.vtcxParent.caller or t.stack
    t.ihpos = t.ihpos or t.stack and t.stack.hpos
    self.__index = self.__index or self
    return setmetatable(t, self)
end
Context.VirtualContext = VirtualContext

return Context
