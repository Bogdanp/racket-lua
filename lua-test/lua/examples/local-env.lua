#lang lua

local old_print = print
local function my_print(...)
    print("my_print:" .. ...)
end

local _ENV = {print = my_print}
print(1, 2)
