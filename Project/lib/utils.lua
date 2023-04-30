--@diagnostic disable: lowercase-global
---@TODO split utils in utils an lpegUtils
lpeg = require"lpeg"

local Object = require"Object"
local Symbol = require"Symbol"

--------------------------------------------------------------------------------
function get(t, k)
    return t[k]
end
function set(t, k, v)
    t[k] = v
end

--------------------------------------------------------------------------------
--lpeg fields
---@TODO : cleaner, use inheritance or something.

lpeg.Cargs = setmetatable ({
},{
    __call = function(self, i, j)
        if j == nil then
            return self[1][i]
        else
            return self[i][j]
--            return lpeg.Cc(i) * self[j] / select      --neat alternative to only store 1 -> i ranges
        end
    end,
    __index = function(self, i)
        self[i] = setmetatable({}, {
            __index = function(self, j)
                if j < i then
                    return lpeg.P''
                else
                    return self[j-1] * lpeg.Carg(j)
                end
            end
        })
        return self[i]
    end,
})
--simple debugging pattern
---@TODO msg.format ? allows much more things
function lpeg.I (msg, ncmt)
    --ncmt = true
    return ncmt and lpeg.P(msg) / print or lpeg.P(function (...) print(msg:format(...)); return true end)
end
--Copies a named pattern.
function lpeg.Cpy(target, source)
    return lpeg.Cg(lpeg.Cb(source or '_'), target)
end

--------------------------------------------------------------------------------
--lpeg switch

local default = Symbol["lpeg.Switch.default"]
local missing = lpeg.P''
lpeg.Switch = setmetatable({
    default = default,
    __call = function(self, case, ...)
        return self[case]:match(case,1, ...)
    end,
    __index = function(self, key)
        return self[default] or missing
    end,
},
{__call = function (self, t)
    return setmetatable(t, self)
end,
})

return lpeg