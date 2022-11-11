#lang lua

a = {}
i = 3
i, a[i] = i + 1, 20
print(i, a[3])

x, y = 1, 2
x, y = y, x
print(x, y)

local a = {}
local i = 3
i, a[i] = i + 5, 20
print(i, a[3])

local x, y = 1, 2
local x, y = y, x
print(x, y)

local function f()
    return 1, 2
end
local x, y = f()
print(x, y)

local x, y = 5, f()
print(x, y)

x, y = 5, f()
print(x, y)

x, y, z = 5, f()
print(x, y, z)
