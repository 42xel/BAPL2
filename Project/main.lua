--[[
Using lua-language-server ( https://marketplace.visualstudio.com/items?itemName=sumneko.lua , https://github.com/LuaLS/lua-language-server )
***
#### quote convention

simple quotes ' ' are used for strings that are only used in internal representation. For the most case, they could be replaced by enums, but are still useful as strings for debug printing.

double  quotes " " are used for strings which appear verbatim (in filename, source code, output, ...)
]]
_README = nil

_INTERPRETER_DEBUG = false
--_INTERPRETER_DEBUG = true
--------------------------------------------------------------------------------
-- -- Figure out the launching directory
local directory = arg[0]:match(".*[/\\]") or ""
package.path = directory .. "lib/?.lua;" .. directory .. "lib/?/init.lua;" .. package.path
if directory ~= "" then
    package.path = directory .. "?.lua;" .. package.path
end
package.cpath = directory .. "lib/?.so;" .. package.cpath

local pt = require "pt".pt
--local utils = require"utils"
--_DEBOGUE = utils.debogue

local parse = require "parser"
local compile = require "compiler"
local run = require "interpreter"

--_DEBOGUE.trace(pt(lpeg))
--------------------------------------------------------------------------------
---@TODO : add an interface of sort
---@TODO : add a repl ?

--------------------------------------------------------------------------------
local input = assert(assert(
    io.open(assert(arg[1], arg[1] and 'r' or "\nUsage:\nlua main.lua myScript.fak"))
):read'a')
io.stderr:write(input)

local ast = parse (input)
io.stderr:write(pt(ast))
--io.stderr:write("MaxOffset", _DEBOGUE.MaxOffset)
io.stderr:write()

local code = compile((ast))
io.stderr:write(pt(code))
io.stderr:write()

io.stderr:write(input)
io.stderr:write("--- run ---\n")
local r = run(code)
io.stderr:write("result")
io.stderr:write(r)
--------------------------------------------------------------------------------
