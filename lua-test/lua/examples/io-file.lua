#lang lua

local path = os.tmpname()
local f = io.open(path, "w")
print(f)
f:write("hello, world!")
f:close()

local inp = io.open(path)
print(inp:read(5))
print(inp:read(5))
print(inp:read(5))
print(inp:read(5))
inp:close()

local inp = io.open(path)
print(inp:read("a"))
inp:close()

local inp = io.open(path)
print(inp:read(100, "a"))
inp:close()

local inp = io.open(path)
print(inp:read(5, "l", 5))
inp:close()

print(os.remove(path))
