local pt = require "pt".pt
parse = require"parser"
compile = require"compiler"
run = require"interpreter"

--------------------------------------------------------------------------------

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
--[[
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