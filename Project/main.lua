local pt = require "pt".pt
parse = require"parser"
compile = require"compiler"
run = require"interpreter"

--------------------------------------------------------------------------------
--TODO : add an interface of sort

local input = io.read()
print(input)
local ast = parse (input)
print(pt(ast))
print()
local code = compile((ast))
print(pt(code))
print()
print(run(code))
--------------------------------------------------------------------------------
