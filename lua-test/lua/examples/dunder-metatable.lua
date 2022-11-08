#lang lua

a = {id = "a"}
b = {id = "b"}
c = {id = "c"}
setmetatable(b, a)
setmetatable(c, b)
print(getmetatable(c) == b)
print(getmetatable(b) == a)
b.__metatable = a
print(getmetatable(c) == a)
print(getmetatable(b) == a)
