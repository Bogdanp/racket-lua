#lang lua

local s = "hello, world!"
print(string.sub(s, 1))
print(string.sub(s, 8))
print(string.sub(s, -1))
print(string.sub(s, -1, -3))
print(string.sub(s, 1, -3))

print(string.byte("hello"))
print(string.byte("hello", 1, -1))

print(pcall(function() string.dump(function() end) end))
print(string.upper("hello!! 123"))
print(string.upper("HELLO!! 123"))
print(string.lower("HELLO!! 123"))

print(string.reverse(""))
print(string.reverse("1"))
print(string.reverse("12345"))

print(string.rep("a", 0))
print(string.rep("a", 1))
print(string.rep("a", 4))
print(string.rep("a", 4, " "))
print(string.rep("abc", 4, ", "))

print(getmetatable(""))
print("1" + 2 + "3")
print(1 + "2" + "3")
print("1" * 5)

local s = "abc"
print(s:reverse())
