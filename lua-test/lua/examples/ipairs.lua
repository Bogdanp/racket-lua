#lang lua

for idx, v in ipairs({1, 2, 3, 4}) do
    print(idx, v)
end

for idx, v in ipairs({a = 1, b = 2}) do
    print("fail")
end

for idx, v in ipairs({1, 2, 3, [5] = 5, a = 6}) do
    print(idx, v)
end

s = {1, 2, 3}
t = {}
setmetatable(t, s)
s.__index = s
for idx, v in ipairs(t) do
  print(idx, v)
end
