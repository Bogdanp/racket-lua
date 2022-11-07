#lang lua

a = {}

function f()
    return a, 50
end

f().x, f()["y"], f()["z"] = 1, 2, {}
print(a.x, a.y)

f()["z"]["a"] = 42
print(f().z.a)

k = "x"
k, f()[k] = "y", 42
print(k, f().x, f().y)

function g()
    return 1, 2
end

k = "x"
k, f()[k] = g()
print(k, f().x, f().y)
