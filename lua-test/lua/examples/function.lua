#lang lua

function add(x, y)
    return x + y
end

print(add(1, 2))

function variadic(...)
    local a, b = ...
    print(a)
    print(b)
end

print("no args:")
variadic()
print("one arg:")
variadic(1)
print("two args:")
variadic(1, 2)

function variadic_add(...)
    local args = {...}
    local accum = 0
    for i = 1, #args do
        accum = accum + args[i]
    end
    return accum
end

print(variadic_add(1, 2, 3, 4))
print(_ENV["variadic_add"])

function f()
    return 1
end

function f()
    return 2
end

print(f())
