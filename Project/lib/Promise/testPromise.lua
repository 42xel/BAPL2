local Promise = require"Promise"

print(Promise)

--local p = Promise(function (h,b)
--    h(7)
--end)

----[[
local pa, b, c
pa = Promise:new()
pa:zen(function (v)
    b = v
end)
print(b, c)
pa:honor(7)
print(b, c)
pa:zen(function (v)
    c = v+4
end)
print(b, c)
--]]