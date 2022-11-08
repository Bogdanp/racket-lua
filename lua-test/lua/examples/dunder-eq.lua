#lang lua

Record = {}
Record.__index = Record
function Record:new(id, o)
    o = o or {}
    o.id = id
    setmetatable(o, Record)
    return o
end

a = Record:new(1, {value = "a"})
b = Record:new(1, {value = "b"})
c = Record:new(2, {value = "c"})
print(a == b)
print(b == a)
print(a == c)
print(c == a)

function Record:__eq(other)
    return self.id == other.id
end
print(a == b)
print(b == a)
print(a == c)
print(c == a)
