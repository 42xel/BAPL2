local dir, test = arg[0]:match"^(.*)/([^/]*)$"
local filePatt = arg[1] or dir

local fileList = {}

do
    local dirPrefix = '.'
    for l in io.popen(([[ls --group-directories-first -R -B %s]]):format(filePatt)):lines'l' do
        if string.sub(l, -1) == ":" then
            dirPrefix = l:sub(1, -2)
        elseif string.sub(l, -4) == ".fak" then
            table.insert(fileList, {dirPrefix, l})
        end
    end
end

for _, f in ipairs(fileList) do
    io.stderr.write("  -------------------------------------")
    io.stderr.write("testing:", f[2])
    --io.stderr.write(table.concat(f, '/'))
    local result = assert(io.popen(([[lua main.lua %s]]):format(table.concat(f, '/')), "r"))
    ---@TODO (once deterministic printing is implemented): pipe stderr and stdout in 2 different temporary files, and compare them to some expected result, with interactive prompt and options.
    for l in result:lines() do
        io.stderr.write(l)
    end
    io.stderr.write("tested:", f[2])
    io.stderr.write("  -------------------------------------")
    io.read()
end
