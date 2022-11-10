#lang lua

for i = 0, 5 do
    print(i)
end

for i = 0, -10, -1 do
    print(i)
end

for i = 0, 10 do
    break
    print("fail")
end
