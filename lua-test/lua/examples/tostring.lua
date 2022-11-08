#lang lua

Wrapper = {}
function Wrapper:new(o)
    o = o or {n = 0}
    setmetatable(o, self)
    return o
end
function Wrapper:__tostring()
    return "Wrapper:" .. self.n
end

print(Wrapper)
print(Wrapper:new({n = 5}))

Named = {__name = "Named"}
foo = {}
setmetatable(foo, Named)
print(Named)
print(foo)
