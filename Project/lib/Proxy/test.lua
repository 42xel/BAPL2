local Proxy = require "Proxy"

local t, gt, st = Proxy()

gt.b = function (t, k)
    print("getter")
end
st.a = function (t, k, v)
    print("setter")
end

t.a = t.b   --getter then setter