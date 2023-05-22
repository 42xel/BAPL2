---@alias ContextElement number|table|nil
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
---@field top ContextElement
---@field parent Context
---@field caller Context
local Context = {__name = "Context",
    validTypes = {
        number = true,
        table = true,
        ["nil"] = true,
    }
}
Context.arrlen = 0
Context.memlen = 0

---@TODO : clean that thing up
--the toplevel parent
Context.parent = {} -- {write = function (_, v) print(v) end, up = print}

Context.caller = nil
---@TODO (for memory cleaning, after VM rewrite) add owner ?
function Context:new(t, arrlen, memlen)
    t = getmetatable(self).new(self, t or {}) --inheritance of sort (here inheriting from Proxy)

    self.arrlen = arrlen or math.max(1, rawlen(t))
    if not memlen then
        memlen = - 1
        while rawget(t, memlen) do
            memlen = memlen - 1
        end
        memlen = - 1 - memlen
    end
    self.memlen = memlen
    t[0] = t
    --t.parent = self   --don't if you don't need I guess ?
    --t.caller = self   --caller ccorresponds to `caller \ callee` ?

    return setmetatable(t, self)
end

function Context:push(...)   --pushes one or several values and returns self, for chaining
    local n, s = select('#', ...), self.arrlen
    self.arrlen = s + n
    table.move({...}, 1, n, 1 + s, self)
    return self
end
function Context:write(v)  --write a single value at the top of the Stack and returns the stack.
    if self.arrlen == 0 then
        print("Context:write(): warning arrlen was zero")
        self.arrlen = 1;
    end
    self.arrlen = math.max(1, self.arrlen)  ---@TODO check whether good/necessary or whether it'd be better to error.
    self.top = v
    return self
end
function Context:rawPeek(n)
    return table.move(self, self.arrlen + 1 - n, self.arrlen, 1, {})
end
function Context:pop(n)    --pops n values and returns them
    n =  n or 1
    if self.arrlen < n then
        error(("can't peek that many (%s) items from the stack (%s)"):format(n, #self), 2)
    end
    local r = self:rawPeek(n)
    self.arrlen = self.arrlen - n
    return table.unpack(r)
end
--@remark ugly code repetition but there is no graceful way to avoid it.
function Context:peek(n)    --peeks at the n top values returns them
    --assert(n <= #self, "can't peek that many items from the stack")
    return table.unpack(self:rawPeek(n or 1))
end
function Context:clean()
    for i = math.max(1,self.arrlen), rawlen(self) do
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
    return self.arrlen
end
--Context.__shl = Context.push
--Context.__shr = Context.write
function Context:__tostring()
    if self.memlen == 0 and self[0] == self then    --litteral Array I guess
        return Array.__tostring(self)
    end
    --print("bla", self.memlen, self[0] == self)
    local r = Context.__name .. ": {\n"
    for i = math.max(self.arrlen, rawlen(self)), 1, -1 do
        r = r .. (i == self.arrlen and "->" or "") .. "\t" .. tostring (self[i]) .. "\n"
    end
    return r .. "}"
end

function Context:_defaultGettersFactory()
    local function __index (stack, k)
        if type(k) ~= 'number' then
            error("Context key, type error:\t" .. type(k) .. " " .. tostring(k))
        end
        assert(-stack.memlen <= k and k <= stack.arrlen, ("Context index(%s) out of bounds (-%s, %s)"):format(k, stack.memlen, stack.arrlen))
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
        --fields
        memlen = self,
        arrlen = self,
        parent = self,
        caller = self,
        --proxy (pseudo) fields
        top = function (stack) return rawget(stack, #stack) end,
        ---@TODO
        --proxy register fields
        --the size of the last litteral array. Needs to be a stack itself I'm afraid.
    }, {__index = function (_, k)
        if type(k) == 'number' then return __index else
            error(("Context index %s invalid"):format(k), 2) end
    end})
    return self._defaultGetters
end
function Context:_defaultSettersFactory()
    local __index = function (stack, k, v)
        if type(k) ~= 'number' then
            error("Context key, type error:\t" .. type(k) .. " " .. tostring(k))
        end
        if  not(-stack.memlen <= k and k <= stack.arrlen) then
            error(("Context index(%s) out of bounds (-%s, %s)"):format(k, stack.memlen, stack.arrlen), 5)
        end
        rawset(stack, k, v)
    end
    self._defaultSetters = setmetatable({
        --fields
        memlen = self,
        arrlen = self,
        parent = self,
        caller = self,
        --proxy fields
        top = function (stack, _top, v) rawset(stack, #stack, v) end,
    }, {__index = function (_, k)
        if type(k) == 'number' then return __index else
            error(("Context index %s invalid"):format(k), 2) end
    end})
    return self._defaultSetters
end

Proxy(Context)
Context()  --initializing by creating an empty Context
Context.__call = nil   --removing an undesired metamethod.
Context.parent = Context:new(Context.parent, 1)
rawset(Context.parent, 'parent', Context.parent)

---@alias ContextGenerator fun(self:Proxy, t?:table, arrlen?:number, memlen?:number):Context
---@diagnostic disable-next-line: cast-type-mismatch
---@cast Context ({new: ContextGenerator})

---@diagnostic disable-next-line: param-type-mismatch
return Context

---@TODO array new and tostring

--------------------------------------------------------------------------------

--[=[
---@TODO make Array proxy ?
---The first (and for now only) data structure of the language.
--@class Array : number[]
---@alias Array ContextElement[]

local Array = {__name = 'Array'}
do  --localising ArraySizes
    local ArraySizes = setmetatable({}, {__mode='k'})   ---we're only storing information around arrays here, we don't want to keep them alive.
    function Array:new (s)
        assert(type(s) == 'number' or type(s) == 'table')
        if type(s) == 'number' then
            local r = setmetatable({--[[size = size or 0]]}, self)
            ArraySizes[r] = s
            return r
        elseif type(s) == 'table' then
            ArraySizes[s] = #s
            return setmetatable(s, self)
        end
    end
    function Array:__len ()
        return ArraySizes[self]
    end
end
---@REMARK we won't do any inheritance with Array, so we don't need to put self.__index ) self in Array.new
---@TODO handle nil values.
--function Array:__index(k)
--    assert(type(k) == 'number')
--    return '_'
--end
--function Array:__newindex(k, v)   --we won't do any inheritance with Array, so we don't need to put self.__newindex ) self in Array.new
--    assert(type(k) == 'number' and 0 < k and k <= #self, ("set(Array, ?, ?) : index invalid or out of bound: %s for array %s of size %d"):format(k, self, #self) )
--    assert(type(v) or true, "set(Array, ?, ?) : incorrect data type")
--    rawset(self, k, v)
--end
---@TODO ponder whether to use brackets or braces. I guess braces for now is best, brackets cuold be used either for splice or to designate an array corresponding to a contiguous segment of memory.
do  --localising depth
    local depth = 0
    function Array:__tostring()
        local r = "\n" .. string.rep("\t", depth) .. "{ " --making room before
        depth = depth + 1

        local ts = {}
        local d = depth
        for i = 1, #self do
            table.insert(ts, tostring(self[i]))
        end
        assert(d == depth)
        r = r .. table.concat(ts, ", ")
        depth = depth - 1
        r = r .. " }" .."\n" .. string.rep("\t", depth) --making room after

        return r:gsub("%}\n(\t*), \n(\t*)%{", "}, \n%1{") --removing double line break from consecutive arrays
                :gsub("%}\n(\t*), ","},\n%1")   --putting commas at the end of lines rather than the beginning
                :gsub("\n\t(\t*) %}", "\n%1}")  --aligning lone closing brackets with their partner.
    end
end

setmetatable(Array, {__call = Array.new})
----@cast Array ({new: fun(t:table):Array}) | fun(t:table):Array

---@alias ArrayNew fun(s:integer|table):Array
---@diagnostic disable-next-line: cast-type-mismatch
---@cast Array ({new: ArrayNew}) | ArrayNew
return Array

--]=]