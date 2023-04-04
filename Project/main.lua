local pt = require "pt".pt
local utils = require"utils"
_DEBOGUE = utils.debogue

parse = require"parser"
compile = require"compiler"
run = require"interpreter"

--_DEBOGUE.trace(pt(lpeg))
--------------------------------------------------------------------------------
--TODO : add an interface of sort

local input = io.read()
print(input)

local ast = parse (input)
print(pt(ast))
print("MaxOffset", _DEBOGUE.MaxOffset)
print()

local code = compile((ast))
print(pt(code))
print()

print(run(code))
--------------------------------------------------------------------------------
