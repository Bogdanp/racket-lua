#lang lua

local function add(x, y)
    return x + y
end
print(add(1, 2))

local function fib(n)
    if n < 2 then
        return 1
    else
        return fib(n - 2) + fib(n - 1)
    end
end
print(fib(8))
