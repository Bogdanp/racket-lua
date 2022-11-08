#lang lua

CustomLength = {}
function CustomLength:__len()
    return self.len
end

o = {len = 5}
setmetatable(o, CustomLength)
print(#o)
