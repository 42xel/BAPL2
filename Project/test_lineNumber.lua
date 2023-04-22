---@diagnostic disable: lowercase-global
local pt = require "pt".pt
local lpeg = require"lpeg"

local utils = require"utils"

local I = utils.debogue.I

parse = require"parser"
compile = require"compiler"
run = require"interpreter"

--------------------------------------------------------------------------------
---[[Test of line count for error reporting

local function inc(x)
    print("inc", x)
    return x + 1
end

local locale = lpeg.locale()
local space = locale.space
local newLine = '\n' * lpeg.Cg(lpeg.Cb("lineNumber") / inc, "lineNumber") * lpeg.Cg(lpeg.Cp(), "lineStart")
local alpha = lpeg.S"ab"
local err = lpeg.Cmt(lpeg.Cb("lineNumber") * lpeg.Cb("lineStart"), function (_, p, n, s, ...)
    --print(_, p, n, s)
    io.stderr:write(string.format("error line: %d character %d\n", n, p - s + 1))
    --os.exit()
end)

local text = lpeg.Cg(lpeg.Cc(1), "lineNumber") * lpeg.Cg(lpeg.Cc(1), "lineStart")
     * lpeg.P{(newLine + space + alpha^1) * lpeg.V(1) + ''} * (-1 + err)


local subjects = {
    "",---[=[
    "a",
    "aabaabbbaabb",
    "aabaabCbaabb",
    [==[aa bab
    aa ab aaa bba
    acb
    aab abb
    ]==],
    [[aa bab
    aa ab aaa bba
    aba
    aab abb
    ]]
    --]=]
}

for _, subject in ipairs(subjects) do
    print(subject)
    print(text:match(subject))
    print()
end

--]]

--------------------------------------------------------------------------------
--[[old (outdated ?) tests of expresions
local subjects = {
    "0",
    " 1 + 1 ",
    "2+3*7",
    "2^2^3",
    "2^2%3",
    "(2+3)*7",
    "4.5 /7 <= 3/5",
    "5.5 /7 < 4/- -5 != 0x.A0e2 ",
}

for _, subject in ipairs(subjects) do
    print(subject)
    local ast = parse (subject)
    print(pt(ast))
    local code = compile((ast))
    print(pt(code))
    print(run(code))
    print'\n'
end
--]]