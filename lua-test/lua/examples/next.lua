#lang lua

v, idx = next({})
print(v, idx)

t = {1, 2, 3}
v, idx = next(t)
print(v, idx)

v, idx = next(t, idx)
print(v, idx)

v, idx = next(t, idx)
print(v, idx)

v, idx = next(t, idx)
print(v, idx)

local _, err = pcall(function()
        v, idx = next(t, 50)
        print(v, idx)
end)
print(err)

-- NOTE: Order not guaranteed so test may be flaky.
local t = {a = 1, b = 2, c = 3}
local k, idx = next(t)
repeat
    print(k, idx)
    k, idx = next(t, idx)
until not idx
