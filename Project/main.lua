package.path = "./lib/?.lua;" .. "./lib/?/init.lua;" .. package.path

local pt = require "pt".pt
--local utils = require"utils"
--_DEBOGUE = utils.debogue

local parse = require"parser"
local compile = require"compiler"
local run = require"interpreter"

--_DEBOGUE.trace(pt(lpeg))
--------------------------------------------------------------------------------
--TODO : add an interface of sort

--TODO update lua
local input = io.read'*a'
print(input)

local ast = parse (input)
print(pt(ast))
--print("MaxOffset", _DEBOGUE.MaxOffset)
print()

local code = compile((ast))
print(pt(code))
print()

print(run(code))
--------------------------------------------------------------------------------
