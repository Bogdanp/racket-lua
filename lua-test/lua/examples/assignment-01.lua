#lang lua

a = {}
a.x = 5
print(a.x)

a.x, a[a.x] = 1, 2
print(a.x, a[5])
