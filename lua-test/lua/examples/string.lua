#lang lua

local s = "hello, world!"
print(string.sub(s, 1))
print(string.sub(s, 8))
print(string.sub(s, -1))
print(string.sub(s, -1, -3))
print(string.sub(s, 1, -3))

print(string.byte("hello"))
print(string.byte("hello", 1, -1))
