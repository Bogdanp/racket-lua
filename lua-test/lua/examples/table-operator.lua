#lang lua

local n = 42
local t0 = { math.min(1, 2, 3) }
print(t0[1])
local t1 = { n / n }
print(t1[1])
local t2 = { x = n / n, 42 }
print(t2.x, t2[1])
local t3 = { [n / n] = 42 }
print(t3[1.0])
