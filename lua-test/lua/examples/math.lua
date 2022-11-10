#lang lua

print(math.abs(5))
print(math.abs(-5))
print(pcall(function() math.abs("hello") end))

print(math.ceil(1.2))
print(math.floor(1.2))

print(pcall(function() math.max() end))
print(math.max(1))
print(math.max(1, 2, 5))

local Comparable = {}
function Comparable.new(id, value)
    local o = { id = id, value = value }
    setmetatable(o, Comparable)
    return o
end
function Comparable:__lt(other)
    return self.value < other.value
end

local a = Comparable.new('a', 1)
local b = Comparable.new('b', 1)
local c = Comparable.new('c', 5)
print(math.max(a, b, c).id)

print(math.min(5, 2, -25))
print(math.min(a, b, c).id)
