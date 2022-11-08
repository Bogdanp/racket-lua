#lang lua

if true then
    print("ok")
end

if false then
    print("fail")
end

if nil then
    print("fail")
end

if nil then
    print("fail")
else
    print("ok")
end

if nil or true then
    print("ok")
end

if nil and true then
    print("fail")
end

if true and nil then
    print("fail")
end

if nil then
    print("fail")
elseif false then
    print("fail")
elseif true then
    print("ok")
else
    print("fail")
end
