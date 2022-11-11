#lang lua

print("hello" .. " " .. "world!")

local ConcatDebug = {}
function ConcatDebug.new(str)
    o = { str = str }
    setmetatable(o, ConcatDebug)
    return o
end
function ConcatDebug:__concat(other)
    print("concat", self.str, "with", other.str)
    return ConcatDebug.new(self.str .. other.str)
end

local a = ConcatDebug.new('a')
local b = ConcatDebug.new('b')
local c = ConcatDebug.new('c')
print(a .. b .. c)
