#lang lua

function range_iter(n)
    return function(s)
        local i = s.current
        if i <= n then
            s.current = i + 1
            return i
        end
    end, {current = 1}
end

for i in range_iter(5) do
    print(i)
end

-- https://www.lua.org/pil/7.1.html
function list_iter(t)
    local i = 0
    local n = #t
    return function()
        i = i + 1
        if i <= n then
            return t[i]
        end
    end
end

for e in list_iter({}) do
    print(e)
end

for e in list_iter({1, 2, 3, 4}) do
    print(e)
end

function ipairs(t)
    return function(t, c)
        c = c + 1
        if t[c] ~= nil then
            return c, t[c]
        end
    end, t, 0
end

for i, v in ipairs({"a", "b", "c", "d"}) do
    print(i, v)
end
