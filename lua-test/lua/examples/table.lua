#lang lua

for k, v in pairs({1, 2, nil, 3}) do
    print(k,v )
end

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
for _, v in pairs(table.pack(table.select(2, 1, 2, 3))) do
    print(v)
end

print("select -2")
for _, v in pairs(table.pack(table.select(-2, 1, 2, 3))) do
    print(v)
end

print("select -1")
for _, v in pairs(table.pack(table.select(-1, 1, 2, 3))) do
    print(v)
end

print("select 0", pcall(function() table.select(0, 1, 2, 3) end))
for i = -1, -9, -1 do
    print("select", i)
    for _, v in pairs(table.pack(table.select(i, 1, 2, 3))) do
        print(v)
    end
end
