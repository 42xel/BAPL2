local lpeg = require"lpeg"

local alpha = lpeg.R('az', 'AZ')

local palNaiveOdd = {
    alpha
}
local palNaiveEven = {
    lpeg.P''
}
for _, g in pairs{palNaiveOdd, palNaiveEven} do
    for _, a in pairs {'a', 'A'} do
        local byte = a:byte()
        for i = byte, byte + 25 do
            g[1] = string.char(i) * lpeg.V(1) * string.char(i) + g[1]
        end
    end
end
palNaiveOdd = lpeg.C(palNaiveOdd) * -alpha
palNaiveEven = lpeg.C(palNaiveEven) * -alpha
local palNaive = palNaiveOdd + palNaiveEven

local function isPalindrome(w)
    for i = 1, #w/2 do
        if w:byte(i) ~= w:byte(-i) then return false end
    end
    return true
end
local palCmt = lpeg.Cmt(alpha^0, function (p, i) local w = p:sub(1,i-1) return isPalindrome(w), w end)

--test
local test = {
    "",
    "a",
    "aa",
    "aba",
    "abba",
    "abbba",
    "blast",
    "baobab",
    "babobab",
    "returner",
    "retrurter",
    "rural",
}

for name, patt in pairs{
    palNaive = palNaive,
    palCmt = palCmt
} do
    print("testing: ", name)
    for i, w in ipairs(test) do
        print(w .. " :", patt:match(w))
    end
end
