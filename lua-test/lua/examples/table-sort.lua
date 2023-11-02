#lang lua

t = {}
table.sort(t)
for k, v in pairs(t) do
    print(k, v)
end

t = {5, 2, 3, 0}
table.sort(t)
for k, v in pairs(t) do
    print(k, v)
end

table.sort(t, function(a, b) return a > b end)
for k, v in pairs(t) do
    print(k, v)
end

t = {"B", "a", "C"}
table.sort(t)
for k, v in pairs(t) do
    print(k, v)
end
