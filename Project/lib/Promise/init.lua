---@TODO  don't forget to use weak tables when applicable.
---@TODO don't forget __gc either. For example, it could be used to raise warnings or errors about ost unresovled promises, and about uncauught arrors.

--A module providing naked proactive promise
--Naked means the user can inspect and temper with their internals as pleases
--Proactive means Promise chaining is done using immediately called callback function, which means chaining occurs depth first (and FIFO)
--This starkly contrast with JavaScript promises which push callback function to a stack in an asynchronous scheduler.
--In particular, we need no scheduler and no coroutines. They might or might not be incorporated at some point, but only as alternative, heavier options.

---@TODO make standalone and publish of sort
--require "utils"
local Object = require"Object"

---@TODO code a Queue in utils 
local Stack = Object:new{}
function Stack:push (...)
    for _,v in ipairs {...} do
        table.insert(self, v)
    end
end
function Stack:pop () --pops one or several, in preserved order (inverse order of popping)
    local n = nil
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
---@diagnostic disable-next-line: duplicate-set-field
function Stack:__call (n)
    return type(n) == "number" and (n > 0 and self[n] or self[#self + n + 1])
end
local Queue = Stack

---@TODO use coroutines as defauult. They're great, they handle error as well !
---@TODO : rename current constructors to 'raw' and make the defauult semantically equivalent to rawConstructor(...).zen(await).
-- a Promise prototype I guess
--@construction
--new
--honor
--betray
--all
--any
--race
--...
--
--@async
--couroutine ? async
--await (yield)
--
--@introspection
--status
--values() --function closure ?
--honor
--betray
    --resolved(Promise) but not settled, see JS.
-- __chaining_files (_zenQueue, _finallyQueue, ...)
--
--@chaining
--_trigger
--_callbackF
--_callbackArgs
--zen
--zenv  --jus returns its argument instead of usnig a fns
--zenp ?
--catch
--finally
--therefore? else? (special then)
--
--@sugar
--Promise.__call : triggers and yield values ?
--PromiseLike.__call : create and trigger, by opposition with Promise.new which only creates ?
local PromiseLike = Object{
    status = "owed", --: "owed" | "honored" | "betrayed"
    __name = "PromiseLike",
}

---@class Promise
---@field _zenQueue any
local Promise = PromiseLike{}

--creates an empty Promise
function Promise:new(t)
    self.__index = self --also serves for chained promise to inherit from each other, notably there queues.
    --Or does it ? queue inheritance is litterally not simple and not in the right way, and there are much simpler way to go about propagating errors.
    t = t or {}
    t._zenQueue = Queue{}

    return setmetatable(t, Promise) --OTOH, no need for a long inheritance chain here, so Promise instead of self.
end
function PromiseLike:__call(executor, ...)    --fn(honor, betray)
    local r = self:new()
    executor(function(...)
        r:honor(...)
    end,
    function(...)
        r:betray(...)
    end, ...)  --TODO pcall ?
end
---triggers the subsequent zens, catches and finallies the promise might have.
---honor is split because some constructors only need that part.
function Promise:_resolveHonored()
    self.status = "honored"
    (function (...)
        for f in self._zenQueue.pop, self._zenQueue do    --TODO zenQueue has to be a queue of promesse somehow.
            f(...)  --TODO pcall ? ok in the __call metamethod
        end
    --     for f in self._finallyQueue.pop, self._finallyQueue do
    --         f() --TODO pcall ? ok in the __call metamethod
    --     end 
    end)(table.unpack(self.values)) --Immediately Invoked Function Expression to optimize the vararg I guess.
    ---@remark Using self.values as opposed to Promise:honor's vararg enables fiddling with the values table from the user if they wish so.
end
function Promise:honor(...)
    if self.status == "owed" then
        self.values = {...}
        self.value = ...

        self:_resolveHonored()
    end
end
--function Promise:betray(msg, ...)
--    if self.status == "owed" then
--        self.error = msg
--        self.status = "betrayed"
--        for _, q in ipairs {self.catchQueue, self.finallyQueue} do
--            for f in q.pop, q do
--                f(...)
--            end
--        end
--    end
--end

function Promise:honored(...)
    return self:new{
        values = {...},
        value = ...,
        status = "honored",
    }
end
---@TODO Promise:betrayed

---
function Promise:all(...)
    local nbOwed = select("#", ...)
    if nbOwed == 0 then return self:honored(...) end

    local r = self:new()
    r.values = {}
    r.value = {}
    for i, p in ipairs{...} do
        p:zen(function (...)
            r.values[i] = {..., status = "honored"}
            r.value[i] = ...
            nbOwed = nbOwed - 1
            if nbOwed == 0 then
                r:_resolveHonored()
            end
        end
    )
    ---@TODO add p:catch
    end
    return r
end

---@TODO zen (Promise) which function/syntax to use ? does
function Promise:zen(cb)
    local st = self.status
    if st == "owed" then
        local r = Promise:new{_callbackF = cb,}
        self._zenQueue:push(r)
        return r
    elseif st == "honored" then
        return Promise(function (h,b)
            h(cb(table.unpack(self.values)))     --TODO pcall ? ok in the __call metamethod ?
        end)
    end
end

function Promise:_callbackF(...)
    return ...
end 
function Promise:__call(...)
    if self.status == "owed" then
        self:honor(self._callbackF(...))
    end
    if self.status == "honored" then
        return table.unpack(self.values)
    end
end

return Promise