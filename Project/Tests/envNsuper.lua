print (_G, _ENV)

A = "global"

local function F0()
    print(A)
end
F0()

do
    local _ENV = {print = print, A = "local0"}
    F0()
end

F0()

local function FF()
    return function ()
        print(A)
    end
end

local F1
do
    local _ENV = {print = print, A = "local1"}
    F1 = FF()
    F1()
end

F1()

print("F2")
local F2 = assert(load(string.dump(F0), nil, nil, {print = print, A = "local2"}))
F2()

print("F3")
local F3
do
    local _ENV = {print = print, string = string, load = load, assert = assert, A = "local3"}
    F3 = assert(load(string.dump(F0)))
end
F3()


--------------------------------------------------------------------------------
--invoquing a super class / a proto type

local Prototype = {__name = "Prototype"}
function Prototype:new(t)
    self.__index = rawget(self, "__index") or self
    t = setmetatable(t or {}, self)
    return t
end

local Type = Prototype:new()
do local proto = Prototype  --you can't do much better than this, it's still clean if you need to copy paste part of the code to use with a different super class/prototype.
    function Type:new(t)
        --...
        t = proto:new(t)
        --...
        return t
    end
end