--@diagnostic disable: lowercase-global
---@TODO split utils in utils an lpegUtils. Other files would load lpeg through lpegUtils

---@alias lpegPatt any

lpeg = require"lpeg"

local Object = require"Object"
local Symbol = require"Symbol"

--------------------------------------------------------------------------------
function get(t, k)
    return t[k]
end
function set(t, k, v)
    t[k] = v
    return v
end

--------------------------------------------------------------------------------
--lpeg fields

--- variants of lpeg.Cargs, capturing a range of arguments, 1,i or i,j.
--- lpeg.Cargs(i, j) expands to (something equivalent to) `lpeg.Carg(i) * lpeg.Carg(i+1) * ... * lpeg.Carg(j-1) * lpeg.Carg(j)`
lpeg.Cargs = setmetatable ({
},{
    __call = function(self, i, j)
        if j == nil then
            return self[1][i]
        else
            return self[i][j]
        end
    end,
    __index = function(self, i)
        -- @TODO the metatable should only be created once (with the table containing `i`).
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

-- Simple debugging pattern.
-- Interject this pattern multiplicatively to print debug infomation in the midle of parsing.
-- @param ncmt : do Not Create Match Time capture. If set to true, msg is printed verbatim and nothing else happen.
-- If set to false, msg is used as a template format acting on the current captures.
---@TODO msg.format ? allows much more things
function lpeg.I (msg, ncmt)
    --ncmt = true
    return ncmt and lpeg.P(msg) / (function (...) io.stderr:write(...) end) or lpeg.P(function (...) io.stderr:write(msg:format(...)); return true end)
end
--Copies a named pattern.
function lpeg.Cpy(target, source)
    return lpeg.Cg(lpeg.Cb(source or '_'), target)
end

--Usually, left folding is more useful because right associative folding comes for free in a way that can't be reproduced left associatively without causing a left recursive pattern
--However, it's not enough for folding a sequence of captures coming from a non recursive pattern (eg. one with a suffix capture).
---lpeg Fold capture right associative
function lpeg.Cfr (patt ,f)
    local function aux(...)
        if 1 < select('#', ...) then
            return f(..., aux(select(2, ...)))
        else
            return ...
        end
    end
    return patt / aux
end

--------------------------------------------------------------------------------
--lpeg switch

---@alias lpegSwitch fun(sw:string, ...:any):any

--local default = Symbol["lpeg.Switch.default"]
local default = {}
local missing = lpeg.P''
---@TODO add error handling to provide the name of the entry at fault upon error
---@type fun(t:{[string]:lpegPatt}):lpegSwitch
---@diagnostic disable-next-line: assign-type-mismatch
lpeg.Switch = setmetatable({
    default = default,
    __call = function(self, case, ...)
        return self[case]:match(case, 1, ...)
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
