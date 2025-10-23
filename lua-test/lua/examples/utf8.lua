#lang lua

print(utf8.char(72, 101, 108, 108, 111))
print(utf8.char(0x48, 0x65, 0x6C, 0x6C, 0x6F))

print(utf8.char(0x4E2D, 0x6587))
print(utf8.char(0x1F600))

print(utf8.len("Hello"))
print(utf8.len(""))

local chinese = utf8.char(0x4E2D, 0x6587)
print(utf8.len(chinese))

print(utf8.len("Hello, World!", 1, 5))

print(utf8.codepoint("H"))
print(utf8.codepoint("Hello", 2))
print(utf8.codepoint("Hello", 1, 3))

local s = utf8.char(0x4E2D, 0x6587)
print(utf8.codepoint(s, 1))
print(utf8.codepoint(s, 4))

print("-- utf8.codes on 'Hi' --")
for p, c in utf8.codes("Hi") do
    print(p, c)
end

print("-- utf8.codes on multi-byte --")
local mb = utf8.char(0x4E2D, 0x6587)
for p, c in utf8.codes(mb) do
    print(p, c)
end

print(pcall(function() utf8.char(-1) end))
print(pcall(function() utf8.char(0x110000) end))
print(pcall(function() utf8.len(123) end))
print(pcall(function() utf8.codepoint(123) end))
