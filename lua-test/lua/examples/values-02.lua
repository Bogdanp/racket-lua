#lang lua

local t = {1, 2, 3}
print(t[1])
print(t[2])
print(t[3])

local function f()
    return 1, 2
end

local t = {x = f()}
print(t.x)

local t2 = {a = t}
print(t2.a.x)

local t3 = {f()}
print(t3[1], t3[2], t3[3])

local t4 = {f(), nil}
print(t4[1], t4[2], t4[3])
