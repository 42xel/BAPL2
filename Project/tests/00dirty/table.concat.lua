local m = {}
local a = setmetatable({3,5,7,4,9}, m)

print(table.concat(a, ", "))
print(a)
m.__name = "Array"
print(a)
function m:__tostring()
    return m.__name .. ": " .. table.concat(self, ", ")
end

a[5] = nil
a[4] = nil
print(#a)
print(a)
print(table.concat(a, ", "))

function m.__len ()
    return 2
end
print(#a)
print(a)
print(table.concat(a, ", "))

a[5] = 11
print(#a)
print(a)
print(table.concat(a, ", "))