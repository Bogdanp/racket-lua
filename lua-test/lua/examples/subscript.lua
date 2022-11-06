#lang lua

t = {"hello"}
print(t[1])
t[1] = "goodbye"
print(t[1])

local k = "hello"
local t = {[k] = "goodbye"}
print(t["hello"])
print(t["goodbye"])
