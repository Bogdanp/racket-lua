#lang lua

local path = os.tmpname()
local file = io.output(path)
file:write("hello\n")
file:write("world!")
file:close()

local file = io.input(path)
for line in file:lines() do
    print(line)
end
file:close()

local file = io.input(path)
for c in file:lines(1) do
    print(c)
end
file:close()

local file = io.input(path)
print(file:seek())
print(file:seek("set", 6))
print(file:read("l"))
print(file:seek("end", 6))
print(file:read("l"))
file:close()
