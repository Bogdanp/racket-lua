#lang lua

a = {}
a.__index = a
function a:__pairs()
    print("__pairs")
    return next, self, nil
end

t = {1, 2, 3}
setmetatable(t, a)
for k, v in pairs(t) do
    print(k, v)
end
