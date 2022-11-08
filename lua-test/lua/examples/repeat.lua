#lang lua

i = 0
repeat
    print(i)
    i = i + 1
until i >= 10

i = 0
repeat
    print(i)
    break
until false

ok = true
repeat
    if not ok then
        print("fail")
    end
until not ok
