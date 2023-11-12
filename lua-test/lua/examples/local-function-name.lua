#lang lua

local function Foo()
    return 1+true
end

print(pcall(function() Foo() end))
