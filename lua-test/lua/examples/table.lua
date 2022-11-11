#lang lua

print(table.pack())
print(table.pack(1, 2, 3))
for k, v in pairs(table.pack(1, 2, 3)) do
    print(k, v)
end
for k, v in pairs(table.pack(1, 2, nil, 3)) do
    print(k, v)
end

print(table.select("#"))
print(table.select("#", 1, 2, 3))

print("select 2")
for _, v in pairs(table.select(2, 1, 2, 3)) do
    print(v)
end

print("select -2")
for _, v in pairs(table.select(-2, 1, 2, 3)) do
    print(v)
end

print("select -1")
for _, v in pairs(table.select(-1, 1, 2, 3)) do
    print(v)
end
