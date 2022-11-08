#lang lua

meta = {}
function meta:__index(k)
    return k..self.suffix
end

loud = {suffix = "!"}
louder = {suffix = "!!"}
loudest = {suffix = "!!!"}
for idx, t in ipairs({ loud, louder, loudest }) do
    setmetatable(t, meta)
    print(t.a)
end
