#lang lua

idx, v = next({})
print(idx, v)

t = {1, 2, 3}
idx, v = next(t)
print(idx, v)

idx, v = next(t, idx)
print(idx, v)

idx, v = next(t, idx)
print(idx, v)

idx, v = next(t, idx)
print(idx, v)

local _, err = pcall(function()
        idx, v = next(t, 50)
        print(idx, v)
end)
print(err)

-- NOTE: Order not guaranteed so test may be flaky.
local t = {a = 1, b = 2, c = 3}
local idx, k = next(t)
repeat
    print(idx, k)
    idx, k = next(t, idx)
until not idx
