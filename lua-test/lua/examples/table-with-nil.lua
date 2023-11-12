#lang lua

print(pcall(function() print({ [nil] = 1 }) end))
