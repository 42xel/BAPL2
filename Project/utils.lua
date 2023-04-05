lpeg = require"lpeg"
Stack = {
    push = function(self, ...)
        for _,v in ipairs {...} do
            table.insert(self, v)
        end
    end,
    pop = function(self, n) --pops one or several, in preserved order (inverse order of popping)
        if n == nil or n == 1 then return table.remove(self, n) end
        local r = {}
        if n <= 0 then n = n + #self end    --0 : pops all, -1 pops all but 1, etc.
        for i = n, 1, -1 do
            r[i] = self:pop()
        end
        return table.unpack(r)
    end,
    unpack = table.unpack,
    --a destructive method which returns a sequence of elements in order of popping (reverse order of indices)
    _unstack_aux = function(self, n, ...)
        if #self > 0 and n > 0 then
            return self:_unstack_aux(n - 1, self:pop(), ...)
        else return ... end
    end,
    unstack = function(self, n)
        return self:_unstack_aux(n or #self)
    end,
    -- s(-1) gets the last element, s(-2) the next to last, etc.
    __call = function(self, n)
        return type(n) == "number" and (n > 0 and self[n] or self[#self + n + 1])
    end,
}
Stack.__index = Stack
setmetatable(Stack, {
    __call = function(self, t) return setmetatable(t, self) end,
})

local debogue = {
    trace = function(...)
        if _DEBOGUE then
            print(...)
        end
    end,
    MaxOffset = 0,
    I = function (tag)
        return lpeg.P(function ()
            print(tag)
            return true
        end)
    end,
}
debogue.ws_suffix = function (_, p)
    debogue.MaxOffset = math.max(debogue.MaxOffset, p)
    return true
end

--TODO : switch case of sort, with RPNfold of arguments
---[[
RPN_Switch = setmetatable ({
    case = {default = {}}, --special keys to refer to other cases
    --["break"] = {}, --special key to break. requires condition ?
    --pass = {},
    --a common metatable to apply to cases to produce the switch
    _casesMeta = {
        __call = function(self, case, ...)
            local caseCode = self[case]
            if type (caseCode) == "function" then
                return caseCode(case, ...)
            end
            local stack = Stack{...}
            local cp = 1
            while cp < #caseCode do
                local code = caseCode[cp]
                if code == RPN_Switch.case.default then
                    self(RPN_Switch.case.default, stack:unpack())
                elseif code == RPN_Switch.case then
                    cp = cp + 1
                    self(rawget(self, caseCode[cp]), stack:unpack())
                elseif type(code) == "function" then
                    cp = cp + 1
                    local nargs = caseCode[cp]
                    if nargs == 0 or nargs == false then code()
                    else
                        if nargs == true then nargs = 0 end --here, 0 means all
                        stack:push(code(stack:pop(nargs)))
                    end
                else
                    stack:push(code)
                end
                cp = cp + 1
            end
        end,
        __index = function(self, key)
            --get the default case if there is one, or do nothing.
            --do nothing is {}, but no need to create a new empty table each time, so might as well use RPN_Switch.case.default
            return rawget(self, RPN_Switch.case.default) or RPN_Switch.case.default
        end
    }
}, {__call = function(self, cases)
    return setmetatable(cases, self._casesMeta)
end})
--]]

--[[
local switch_example = RPN_Switch{
    a = {3},    --returns value 3
    b = {"hello", "world", print, 2},  --executes print("hello", "world")
    c = {RPN_Switch.case, "a", RPN_Switch.case.default},  --executes the code from case "a" then from case default.
    d = {print, true},  --prints the full stack.
    [RPN_Switch.case.default] = function(bla, ...) error("missing case: " .. tostring(bla)) end,   --default case.
}
print("testing case: " .. 'a' .. ":\t", switch_example("a"))
print("testing case: " .. 'b' .. ":\t", switch_example("b"))
print("testing case: " .. 'c' .. ":\t", switch_example("c"))
print("testing case: " .. 'd' .. ":\t", switch_example("d", "bla", "bla", "bla"))
print("testing case: " .. 'e' .. ":\t", switch_example("e"))
--]]

--------------------------------------------------------------------------------
return {
    debogue = debogue
}