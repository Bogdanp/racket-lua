#lang lua

function f()
    return 1, 2
end

local x, y = f()
print(x, y)
print(x + f() + f())

function g()
    return 0, f()
end
print(g())

local a, b, c, d = g()
print(a, b, c, d)

local a, b, c, d = g(), nil
print(a, b, c, d)
