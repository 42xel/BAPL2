---@diagnostic disable: lowercase-global
local pt = require "pt".pt
local lpeg = require"lpeg"

local utils = require"utils"

--------------------------------------------------------------------------------
--basic principles of (named) captures scoping
--[[beware grouping when capturing

patt1 = lpeg.Cc"Hello"
patt2 = lpeg.Cc"World" / print
print((patt1 * patt2):match'')
--]]
--[[using named captures to circumvent the issue

patt1 = lpeg.Cg(lpeg.Cc"Hello", "pre")
patt2 = lpeg.Cb"pre" * lpeg.Cc"World" / print
print((patt1 * patt2):match'')
--]]
--[[a less modular solution not using named captures

patt1 = lpeg.Cc"Hello"
patt2 = patt1 * lpeg.Cc"World" / print
print(patt2:match'')
--]]

--------------------------------------------------------------------------------
--[[
back capture [...] produces the values produced by the most recent group capture named name (where name can be any Lua value).
Most recent means the last complete outermost group capture with the given name.
A Complete capture means that the entire pattern corresponding to the capture has matched.
An Outermost capture means that the capture is not inside another complete capture.

As a consequence, captures "consume" all captures, including named ones. 
So you can't just mindlessly reduce a subpattern if you want to keep track of a value accross its super pattern.
A couple differnet solutions :
* use match time capture, to leaisurly use border effect, full subject external anaysis and whatnot
* use table capture everywhere, pass along a compile state table, have capture functions unpack it a nd return it.
* use named capture everywhere, to leisurely recompose them.
* put ine number into or around the AST

solution 1 is somewhat inelegant, maybe one want to do more than just reporting linenumber, and then 
solution 3 is not as easy and more restrictive as it initially sounds
solution 2 sounds like it restrict to only function capture (but then again so do the other ones)
solution 4 sounds like best.

]]

--[[

patt1 = lpeg.Cg(lpeg.Cc"Hello", "pre")
patt2 = lpeg.Cb"pre" * lpeg.Cc"World" * lpeg.Cg(lpeg.Cc"goodbye", "post") / print
print((patt1 * patt2 * lpeg.Cb"post"):match'')  --lua: test_Cb.lua:33: back reference 'post' not found

--one solution is to only ever deal with table captures.
--]]
--[[

patt1 = lpeg.Cg(lpeg.Cc"Hello", "pre")
patt2 = lpeg.Cb"pre" * lpeg.Cc"World" / print * lpeg.Cg(lpeg.Cc"goodbye", "post")
print((patt1 * patt2 * lpeg.Cb"post"):match'')  --lua: test_Cb.lua:33: back reference 'post' not found
--]]

--[[

patt0 = lpeg.Cg(lpeg.Cc"howdy", "through") * lpeg.Cc"Hello" * lpeg.Cc"World"
patt1 = patt0 / print
patt2 = patt1 * lpeg.Cb"through" / print
print(patt2:match'')  --lua: test_Cb.lua:49: back reference 'through' not found
--]]
--[[

patt0 = lpeg.Cg(lpeg.Cc"World", "post") * lpeg.Cg(lpeg.Cc"dear", "through") * lpeg.Cg(lpeg.Cc"Hello", "pre") * lpeg.Cg(lpeg.Cb"pre" * lpeg.Cb"post", "args") * (lpeg.Cb"args" / print)
--patt1 = lpeg.Cc"Hi" * lpeg.Cg(lpeg.Cb(1) * lpeg.Cc"Hello" * lpeg.Cc"World") / print
print(patt0:match'')  --lua: test_Cb.lua:49: back reference 'through' not found
--]]

--[[group capture don't capture nest capture
patt1 = lpeg.Cg(lpeg.Cg(lpeg.Cc(14), "nested"), "nest") * lpeg.Ct(lpeg.Cb"nest") / pt / print
print(patt1:match'')
]]

patt = lpeg.Cg(lpeg.Cc"hello", 1) * (lpeg.Cg(lpeg.Cc"howdy", 1) * lpeg.Cg(lpeg.Cc(nil), 1) ) * lpeg.Cg(lpeg.Cc"world", 2) * lpeg.Cb(1) * lpeg.Cb(2)
print(patt:match'')

--------------------------------------------------------------------------------
--[[attempt to reproduce somthing wrong
---@TODO the issue stems from the / operator, which consumes all captures, including named ones.
---@TODO sounds like anon grouping might be the way ?
---@TODO special non consuming pattern functorintorino-factory of sort ?



local locale = lpeg.locale()
local function inc(...)
    print("inc", ...)
    local x = ...
    return x + 1
end

---@TODO : store lineNumber and lineStart inside or around the AST for reporting and syntax highlight
local newLine = '\n' * lpeg.Cg(lpeg.Cb("lineNumber") / inc, "lineNumber") * lpeg.Cg(lpeg.Cp(), "lineStart")
local ws = newLine + locale.space    --we might need ws or ws^1 in some places
local ws_ = ws^0
--returns a pattern raising an error
local function err(msg)
    return lpeg.Cmt(lpeg.Cb("lineNumber") * lpeg.Cb("lineStart"), function (subject, p, n, s, ...)
        io.stderr:write(string.format("error in <input>:%d:%d\t", n, p - s + 1))    --TODO use filename
        io.stderr:write(msg .. '\n')
        os.exit()
    end)
end

local stats_ = lpeg.Cl{(lpeg.P'a'+ ws) * lpeg.V(1) + ''}
local filePatt = lpeg.Cg(lpeg.Cc(1), "lineNumber") * lpeg.Cg(lpeg.Cc(1), "lineStart") * ws_ * stats_ * (-1 + err"file: eof expected.")

print (filePatt:match(io.read"*a"))

--]]
--------------------------------------------------------------------------------
