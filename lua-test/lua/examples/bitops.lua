#lang lua

print(~1)
print(~.0)
pcall(function()
        print(~.5)
end)

print(1 | 2)
print(1 & 2)
print(2 & 2)
print(1 ~ 5)
print(1 << 1)
print(1 << 8)
print(256 >> 1)
print(256 >> 8)
