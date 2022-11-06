#lang lua

a = 0
while true do
    if a > 10 then
        break
    end
    print(a)
    a = a + 1
end
