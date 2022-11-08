#lang lua

p, t, idx = pairs({1, 2, 3})
print(p, t, idx)
idx, v = p(t, idx)
print(p, t, idx)
idx, v = p(t, idx)
print(p, t, idx)
idx, v = p(t, idx)
print(p, t, idx)
idx, v = p(t, idx)
print(p, t, idx)

for k, v in pairs({a = 1, b = 2, c = 3}) do
    print(k, v)
end
