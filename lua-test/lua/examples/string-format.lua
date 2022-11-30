#lang lua

print(string.format("%d", 123456))
print(string.format("%q", true))
print(string.format("%q", false))
print(string.format("%q", nil))
print(pcall(function() string.format("%q", {}) end))
print(string.format("%q %q", 1, 1.23))
print(string.format("%q", "hello, world!"))
print(string.format("%q", [==[
  hello, "world!"
]==]))
print(string.format("%80q", 1.23))
