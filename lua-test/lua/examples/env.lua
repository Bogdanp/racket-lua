#lang lua

print(a)
print(_ENV)
_ENV["a"] = 42
_ENV["b"] = 123
local a = 1
print(a)
print(b)

local old_print = print
function my_print(a)
    old_print("my_print:" .. a)
end
_ENV["print"] = my_print
print(a)
