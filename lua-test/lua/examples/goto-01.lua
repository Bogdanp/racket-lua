#lang lua

a = 0

::a::
print(a)
a = a + 1
if a < 10 then
    goto a
end
