local pt = require "pt".pt
local lpeg = require "lpeg"

local utils = require "utils"

local _Gmeta = utils.set_GlpegShortHands"V"

--------------------------------------------------------------------------------
--utils

-- a function generator which generates node functions, used to parse captures as nodes of the ast
-- Usage: look at examples
-- nodeGenerator{tag = "tag", field1, field2}   : generates based on a template
-- nodeGenerator(predicate, number, {args1}, {args2})   : uses a predicate on the numberth argument to chose another node function
-- nodeGenerator(number)    : chooses the numberth argument
-- nodeGenerator(number, args)  : generate a function with the numberth argument as the starting node (as opposed to creating a new one)
--
--TODO (after OO) incorporate lineCount
--TODO : document usage and returned function
local nodeGenerator = setmetatable({_sel = 1}, {
    __call = function (self, t, n, argst1, argst2, ...)
        if type(t) == "table" then
            local sel = self._sel
            return function (...)
                local r = select(sel, {}, ...)
                for k, v in pairs(t) do
                    local tk = type(k)
                    if tk == "number" then  --variable stuff, such as exp1, exp2, stat1, stat2 ...
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
                    return self[argst1](...)
                else
                    return self[argst2](...)
                end
            end
        elseif type(t) == "number" then
            self._sel = t + 1
            local r = self(n, argst1, argst2, ...)
            self._sel = 1
            return r
        elseif type(t) == "nil" then
            local sel = self._sel
            return function(...) return select(sel, {}, ...) end--???
        else
            error("nodeGenerator() : first argument must be a table, a function or a number. Got "
                .. tostring(t)
                .. (type(t) == "userdata" and " . You might be using something undefined (_G shenanigans)." or ''))
        end
    end,
    __index = function(self, packedTable)
        self[packedTable] = self(table.unpack(packedTable))
        return self[packedTable]
    end,
})

--------------------------------------------------------------------------------
--node capture functions

local function isNodeEmpty(n)
    return n == nil or n.tag == "void"
end
local emptyNode = {tag = "void"}

--[[old nodes examples
local function nodeSeq(stat1, stat2)
    return isNodeEmpty(stat2) and stat1 or {tag = "seq", stat1 = stat1, stat2 = stat2}
end
local function nodeBinop(a, op, b)
    return {
        tag = "binop",
        op = op,
        exp1 = a,
        exp2 = b,
    }
end
--all in all, 30 well invested lines of codes in the function generator to save 10 uses of the 'function' keywords, and give the linter a harder time.
--]]
--TODO : add comments?
--TODO see whether isNodeEmpty is really necssary/helpful there.

local nodeSeq = nodeGenerator(isNodeEmpty, 2, {1}, {{tag = "seq", "stat1", "stat2"}})
local nodeRet = nodeGenerator{tag = "return", "exp"}
local nodePrint = nodeGenerator{tag = "print", "exp"}
local nodeAssign = nodeGenerator{tag = "assign", "id", "exp"}
local nodeNum = nodeGenerator{tag = "number", "val"}
local nodeVar = nodeGenerator{tag = "variable", "var"}

local nodeBinop = nodeGenerator{tag = "binop", "exp1", "op", "exp2"}
local nodeFoldBinop = nodeGenerator(isNodeEmpty, 2, {1}, {2, {tag = "binop", "exp1"}})
local nodeFoldBinopSuffix = nodeGenerator{tag = "binopSuffix", "op", "exp2"}
local nodeUnaryop = nodeGenerator{tag = "unaryop", "op", "exp"}    --local nodeUnaryop = nodeGenerator(isNodeEmpty, 2, {1}, {tag = "unaryop", "op", "e"})

--Sizeable issue, in a < b < c, expression b is duplicated.
--TODO : rework comparisons to do something smarter (and do it later) to compute middle terms only once.
--TODO For later, when I ll know flow, andmemory alloted to interpreter
--TODO Or maybe when I ll use leftValues
local function foldCompChain(t)
    --a < b < c will ultimately be transformed into (a<b) and (b<c)
    if #t == 1 then return t[1] end
    local r = {
        tag = "varop",
        eStack = Stack{},
        clause = "conjonction",
    }
    for i = 2, #t, 2 do
        r.eStack:push(nodeBinop(t[i-1], t[i], t[i+1]))
    end
    return r
end

--------------------------------------------------------------------------------
--elementary patterns

local locale = lpeg.locale()
local function inc(x)
    return x + 1
end

--spaces
--TODO : store lineCount and lastLineStart inside or around the AST for reporting and syntax highlight

local lineComment = "#" * (P(1) - "\n")^0
local comment = lineComment
local newLine = '\n' --  * lpeg.Cg(lpeg.Cb("lineCount") / inc, "lineCount") * lpeg.Cg(Cp(), "lastLineStart")
--TODO put spaces inside the grammar
--local ws = V"ws"
--local ws = V"ws_"
local ws = newLine + locale.space + comment    --we might need ws or ws^1 in some places
local ws_ = ws^0

--TODO make msg an object ??
--TODO use a patt argument for nested/chained errors ??
--returns a pattern raising an error
--local function err(msg)
--    return Cmt(lpeg.Cb("lineCount") * lpeg.Cb("lastLineStart"), function (subject, p, n, s, ...)
--        io.stderr:write(string.format("error in <input>:%d:%d\t", n, p - s + 1))    --TODO use filename
--        io.stderr:write(msg .. '\n')
--        os.exit()
--    end)
--end
local function err(msg)
    return Cmt('', function (subject, p)
        local lineStart
        local _, lineNumber = subject:sub(1, p):gsub("()\n", function (pos) lineStart = pos end)
            io.stderr:write(string.format("error in <input>:%d:%d\t", lineNumber + 1, p - lineStart + 1))    --TODO use filename
            io.stderr:write(msg .. '\n')
            os.exit()
    end)
end

local comma = S'.'

local Assign_ = '=' * ws_

local digit = R'09'
local digits = digit^0
local hexdigit = R('09', 'af', 'AF')
local hexdigits = hexdigit^0
local function numeralCapture(digit, comma)
    return (digit^0 * (comma * digit^0)^-1) - ((comma+'')*-(digit+comma))
end
--I might want to have coding numeral stuck to variable identifiers or very special operators, so no spaces at the end.
local numeral = ('0' * lpeg.S'xX' * numeralCapture(hexdigit, comma) + numeralCapture(digit, comma) * (S'eE' * digit^1)^-1) / tonumber / nodeNum

local alpha = R('az', 'AZ')
local alphanum = alpha+digit

--no spaces, potentially, things like field access need to be stuck to the ID
--the possibility of no spaces before seems more important though
local ID = C((alpha + '_') * (alphanum + '_')^0)
local var = ID / nodeVar

local OP_ = '(' * ws_
local CP_ = ')' * ws_
local OB_ = '{' * ws_
local CB_ = '}' * ws_
local SC_ = ';' * ws_

local ret_ = "return" * ws_
local printStat_ = '@' * ws_

--------------------------------------------------------------------------------
--elaborate patterns

local function infixOpCapture(opPatt, abovePattern)
    return lpeg.Cf(abovePattern * (opPatt * abovePattern / nodeFoldBinopSuffix)^0, nodeFoldBinop) 
end
local function infixOpCaptureRightAssoc(opPatt, selfPattern, abovePattern)  --set self to above to have a non asociative binary op.
    --return lpeg.Cg(abovePattern, '_') * lpeg.Cg(lpeg.Cb('_') * (opPatt * selfPattern / nodeFoldBinopSuffix) / nodeFoldBinop, '_')^-1 * lpeg.Cb('_') --overkill.
    return abovePattern * (opPatt * selfPattern / nodeFoldBinopSuffix)^-1 / nodeFoldBinop
end
local function unaryOpCapture(opPatt, selfPattern, abovePattern)  --allows chaining. set self to above to disallow
    return abovePattern + opPatt * ws_ * abovePattern / nodeUnaryop + opPatt * ws^1 * selfPattern / nodeUnaryop --not allowing ++ , -- or +- , but allowing - -
end
local function infixCompChainCapture(opPatt, abovePattern)
    return lpeg.Ct(abovePattern * (opPatt * abovePattern)^0) / foldCompChain
end

--TODO : make every statement expression.
-- a list of constructs useable to build expression, from highest to lowest priorityst+2)))
local exp_ = Stack{"exp",
    (numeral + var) * ws_ + OP_ * exp * CP_, --primary
}
exp_:push(infixOpCaptureRightAssoc(lpeg.C(lpeg.S'^') * ws_, V(#exp_+1),  V(#exp_))) --power
exp_:push(unaryOpCapture(lpeg.C(lpeg.S'+-'), V(#exp_ + 1), V(#exp_))) --unary +-
exp_:push(infixOpCapture(lpeg.C(lpeg.S'*/%') * ws_, V(#exp_))) --multiplication
exp_:push(infixOpCapture(lpeg.C(lpeg.S'+-') * ws_, V(#exp_))) --addition
exp_:push(infixCompChainCapture(lpeg.C(lpeg.S'<>' * lpeg.P'='^-1 + lpeg.S'!=' * '=') * ws_, V(#exp_))) --comparison
--TODO : ponder and discuss priority. My idea : logical operator => very low prio.
--TODO : for example, comparisons create booleans, so having logical operators of lower precedence alow to combine them wihout parentheses.
exp_:push(unaryOpCapture(C'~', V(#exp_ + 1), V(#exp_)))    --unary not.

exp_.exp = V(#exp_)

exp_ = P(exp_)

--TODO : use infixOpCaptureRightAssoc and modify nodeAssign so as to be able to chain assignement (C/C++/js/... style). Issue : emptying the stack if the value is not used

local stats_ = {"stats",
    stat = block
        + ID * ws_ * Assign_ * exp_ / nodeAssign
        + ret_ * exp_ / nodeRet
        + printStat_ * exp_ / nodePrint
        + SC_^(-1) * lpeg.Cc(emptyNode),
    stats = (stat * (SC_ * stats)^-1 / nodeSeq),
    block = OB_ * stats * (CB_ + err"block: missing brace"),
}
stats_ = P(stats_)

local filePatt = 
    --lpeg.Cg(lpeg.Cc(1), "lineCount") * lpeg.Cg(lpeg.Cc(1), "lineStart") * 
    ws_ * stats_
     * (-1 + err"file: eof expected.") --TODO better error msg
local function parse (input)
    return filePatt:match(input)
end
--------------------------------------------------------------------------------
return parse