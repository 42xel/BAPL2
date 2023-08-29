local Object = require"Object"


---@alias AST table
--------------------------------------------------------------------------------

local MetaNode = Object{__name = "MetaNode"}
local Node = MetaNode{__name = "Node",tag = 'void', _sel = 1}
--[[
a function generator which generates node functions, used to parse captures as nodes of the ast
***
### Usage: 
look at examples

    Node()  -- generates an empty node.
    Node{tag = 'tag', field1, field2}   -- generates based on a template
    Node(predicate, number, {args1}, {args2})   -- uses a predicate on the number-th argument to chose another node function
    Node(number)    -- when 0 < n : chooses the number-th argument
    Node(number, args)  -- generate a function with the number-th argument as the starting node (as opposed to creating a new one)
***
#### TODO
- (after OO) incorporate lineCount. Do you really need them ?
- use meta programming
- : document usage and returned function
***
### Node Templates:
    {tag = 'tag', [0] = vararg, field1, field2}
#### TODO
        more examples, with explanations
]]
function MetaNode:__call (t, n, argst1, argst2, ...)
    if type(t) == "table" then
        local sel = self._sel
        return function (...)
            local r = select(sel, {}, ...)
            for k, v in pairs(t) do
                local tk = type(k)
                if k == 0 then
                    r[v] = {select(1 + #t, ...)} --variable stuff of variable number.
                elseif tk == "number" then  --variable stuff, such as exp1, exp2, stat1, stat2 ...
                    r[v] = select(k, ...)
                elseif tk == "string" then  --constant stuff, such as tag
                    r[k] = v
                end
            end
            return r
        end
    elseif type(t) == "function" then
        return function (...)
            if t(select(n, ...)) then
                return Node[argst1](...)
            else
                return Node[argst2](...)
            end
        end
    elseif type(t) == "number" then
        self._sel = t + 1
        local r = self(n, argst1, argst2, ...)
        self._sel = 1
        return r
    elseif type(t) == "nil" then
        local sel = self._sel
        return function(...) return ({{}, ...})[sel] end    --not using select so that only one result is returned.
    else
        error("MetaNode:__call() : first argument must be a table, a function or a number. Got "
            .. tostring(t)
            .. (type(t) == "userdata" and " . You might be using something undefined (_G shenanigans)." or ''))
    end
end
Node.empty = {tag = 'void'}
function MetaNode:__index(packedTable)
    if packedTable == nil then return io.stderr:write("Warning packedTable nil") and Node.empty
    elseif type(packedTable) ~= 'table' then return io.stderr:write("Warning packedTable not table") end

    self[packedTable] = self(table.unpack(packedTable))
    return self[packedTable]
end
function Node.isEmpty(self)
    return self == nil or self.tag == 'void'
end

Node.nodeNum = Node{tag = 'number', 'val'}
Node.nodeStr = Node{tag = 'string', 'val'}

Node.nodeBinop = Node{tag = 'binop', 'exp1', 'op', 'exp2'}
---@TODO see whether isNodeEmpty is really necssary/helpful there.
Node.nodeFoldBinop = Node(Node.isEmpty, 2, {1}, {2, {tag = 'binop', 'exp1'}})
Node.nodeFoldBinopSuffix = Node{tag = 'binopSuffix', 'op', 'exp2'}
Node.nodeUnaryop = Node{tag = 'unaryop', 'op', 'exp'}    --local nodeUnaryop = nodeGenerator(isNodeEmpty, 2, {1}, {tag = 'unaryop', "op", "e"})

Node.indexed = Node{tag = 'indexed', 'ref', 'index'}
Node.nodeAssign = Node{tag = 'assign', "lhs", "exp"}
Node.nodeBlock = Node{tag = 'block', 'content'}
Node.nodeReturn = Node{tag = 'return', "exp"}

Node.fun = Node{tag = 'fun', 'ref', 'param'}
--Node.funLHSparams = Node{tag = 'params', [0] = "list"}
Node.fundef = Node{tag = 'fundef', 'default', 'sbody'}--, 'ctx'}

return Node
