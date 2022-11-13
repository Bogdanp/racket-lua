#lang lua

local function f(x)
    return x ^ 2
end

print((nil or f)(4))

local o = {}
function o:ok(v)
    return v == "abc"
end
print((true and (nil or o):ok("abc")))
print((true and (nil or o):ok("abc")) and false)

local o = {}
function o:id(v, ...)
    if v ~= nil then
        return v, o:id(...)
    end
end
print(o:id(1, ((o.id)(o) or true)))

print(
    (function() return false end)(1) or
    (function(x) return x == 2 end)(2)
)
