#lang lua

local t = json.decode'{"o": {"a": [1, 2, 3], "b": 42, "c": {}, "d": false}}'
print(t)
print(t.o)
print(t.o.a, t.o.b, t.o.c, t.o.d)
print(json.encode(t))
