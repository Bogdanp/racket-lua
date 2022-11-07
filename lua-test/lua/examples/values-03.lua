#lang lua

local function f()
    return 1, 2
end

a, b = 1, 2, f()
print(a, b)
