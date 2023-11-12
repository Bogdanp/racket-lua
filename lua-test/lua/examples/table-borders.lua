#lang lua

print(#{})
print(#{a = 1, b = 2})
print(#{10, 20, 30, 40, 50})
print(#{10, 20, 30, nil, 50})
print(#{nil, 20, 30, nil, nil, 60, nil})
print(#{nil, 20, 30, 40, 50, a = 1, b = 2})
print(#{[100] = 1})
print(#{1, [100] = 2})

local t = {nil, 10, 20, nil}
print(#t)
table.insert(t, 20)
table.insert(t, 30)
print(#t)
