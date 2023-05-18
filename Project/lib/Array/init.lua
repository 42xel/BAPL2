---@TODO make Array proxy ?
---The first (and for now only) data structure of the language.
--@class Array : number[]
---@alias Array IntStackElement[]

local Array = {__name = 'Array'}
do  --localising ArraySizes
    local ArraySizes = setmetatable({}, {__mode='k'})   ---we're only storing information around arrays here, we don't want to keep them alive.
    function Array:new (s)
        assert(type(s) == 'number' or type(s) == 'table')
        if type(s) == 'number' then
            local r = setmetatable({--[[size = size or 0]]}, self)
            ArraySizes[r] = s
            return r
        elseif type(s) == 'table' then
            ArraySizes[s] = #s
            return setmetatable(s, self)
        end
    end
    function Array:__len ()
        return ArraySizes[self]
    end
end
---@REMARK we won't do any inheritance with Array, so we don't need to put self.__index ) self in Array.new
---@TODO handle nil values.
--function Array:__index(k)
--    assert(type(k) == 'number')
--    return '_'
--end
--function Array:__newindex(k, v)   --we won't do any inheritance with Array, so we don't need to put self.__newindex ) self in Array.new
--    assert(type(k) == 'number' and 0 < k and k <= #self, ("set(Array, ?, ?) : index invalid or out of bound: %s for array %s of size %d"):format(k, self, #self) )
--    assert(type(v) or true, "set(Array, ?, ?) : incorrect data type")
--    rawset(self, k, v)
--end
---@TODO ponder whether to use brackets or braces. I guess braces for now is best, brackets cuold be used either for splice or to designate an array corresponding to a contiguous segment of memory.
do  --localising depth
    local depth = 0
    function Array:__tostring()
        local r = "\n" .. string.rep("\t", depth) .. "{ " --making room before
        depth = depth + 1

        local ts = {}
        local d = depth
        for i = 1, #self do
            table.insert(ts, tostring(self[i]))
        end
        assert(d == depth)
        r = r .. table.concat(ts, ", ")
        depth = depth - 1
        r = r .. " }" .."\n" .. string.rep("\t", depth) --making room after

        return r:gsub("%}\n(\t*), \n(\t*)%{", "}, \n%1{") --removing double line break from consecutive arrays
                :gsub("%}\n(\t*), ","},\n%1")   --putting commas at the end of lines rather than the beginning
                :gsub("\n\t(\t*) %}", "\n%1}")  --aligning lone closing brackets with their partner.
    end
end

setmetatable(Array, {__call = Array.new})
----@cast Array ({new: fun(t:table):Array}) | fun(t:table):Array

---@alias ArrayNew fun(s:integer|table):Array
---@diagnostic disable-next-line: cast-type-mismatch
---@cast Array ({new: ArrayNew}) | ArrayNew
return Array