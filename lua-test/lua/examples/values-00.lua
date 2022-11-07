#lang lua

function f(a, b)
    print(a, b)
end

f(1)
f(1, 2)
f(1, 2, 3)

function g()
    return 1, 2
end

f(g())
f(g(), g())
f(g(), g(), g())

function h()
end

print(h())

local x, y = g(), g(), nil
print(x, y)

local x, y = g()
print(x, y)

function i()
    return function()
        return 1
    end, function()
        return 2
    end
end

print(i())
local fa = i()
print(fa())

local fa, fb, fc = i()
print(fa())
print(fb())
print(fc)

local function j(x, y, ...)
    local z = ...
    print(x, y, z, ...)
end

j(1, 2)
j(1, 2, 3)
j(1, 2, 3, 4)
j(1, 2, 3, 4, 5)

local function k(...)
    local t = {...}
    print("k:", #t)
    print(t)
    print(t[1])
    print(t[2])
end

k()
k(1, 2)
k(1, g())
k(g(), g())
k(g())
