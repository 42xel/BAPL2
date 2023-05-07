---@diagnostic disable: assign-type-mismatch
local IntStack = require "IntStack"
local pt = require"pt".pt

--print("pt(IntStack.sizes)", pt(IntStack.sizes))

--local a = IntStack:new{"hello", "world"}
local a = IntStack:new{"hello", "world"}

--print(a:get"top")
print(a.top)
print(pt(a))
print(#a)
print(a[#a])

a.top = "sunshine!"
print(a.top)
print(pt(a))

print(a)
_ = a << "how" << "are" << "you" << "my" << "dear?"

print(a)
a.len = 4
print(a)

print(a:pop(2))
print(a)

a:write"was"
print(a)

local b = IntStack:new()
b:write"Ciao!"
print(b)