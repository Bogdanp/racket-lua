#lang lua

if pcall(function() return 1 end) then
    print("ok")
end

if pcall(function() error(404) end) then
    print("fail")
else
    print("ok")
end

local status, err = pcall(function() a = 'a' + 1 end)
print(status)
print(err)

local status, err = pcall(function() error({code = 1}) end)
print(status)
print(err.code)

local status, err = pcall(function() error({code = 1}, 2) end)
print(status)
print(err.code)

local status, err = pcall(function(x) error(x) end, 5)
print(status)
print(err)
