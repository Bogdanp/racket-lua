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

print(select("#"))
print(select("#", 1, 2, 3))

print("select 2")
for _, v in pairs(table.pack(select(2, 1, 2, 3))) do
    print(v)
end

print("select -2")
for _, v in pairs(table.pack(select(-2, 1, 2, 3))) do
    print(v)
end

print("select -1")
for _, v in pairs(table.pack(select(-1, 1, 2, 3))) do
    print(v)
end

print("select 0", pcall(function() select(0, 1, 2, 3) end))
for i = -1, -9, -1 do
    print("select", i)
    for _, v in pairs(table.pack(select(i, 1, 2, 3))) do
        print(v)
    end
end

local t = {}
table.insert(t, 'a')
table.insert(t, 'b')
table.insert(t, 'c')
for k, v in pairs(t) do
    print(k, v)
end

table.insert(t, 2, 'd')
for k, v in pairs(t) do
    print(k, v)
end

print(pcall(function() table.insert(t, 0, 'e') end))
print(pcall(function() table.insert(t, 6, 'e') end))

table.remove(t)
for k, v in pairs(t) do
    print(k, v)
end

table.remove(t, 2)
for k, v in pairs(t) do
    print(k, v)
end

print(pcall(function() table.remove(t, 0) end))
print(pcall(function() table.remove(t, 3) end))

table.insert(t, 'c')
print(table.concat(t))
print(table.concat(t, ' '))
print(table.concat(t, ' ', 2))
print(table.concat(t, ' ', 5))

print(pcall(function() t[nil] = 5 end))
