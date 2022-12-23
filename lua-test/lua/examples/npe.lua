#lang lua

print(pcall(function() idontexist(1, 2, 3) end))
