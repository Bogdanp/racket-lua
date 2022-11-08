#lang lua

i = 0
while i < 10 do
    print(i)
    i = i + 1
end

i = 0
while i < 10 do
    print(i)
    break
end

while nil do
    print("fail")
end
