#lang lua

print"hello"

local o = {}
function o.show(v)
    print(v)
end

o.show"hi"
o.show{}
o["show"]"hi"

local metao = {}
metao.__index = metao
function metao:show(v)
    print(self, v)
end

local w = { inner = o }
setmetatable(w, metao)

w.inner["show"]"hi"
w:show"meta hi"
w:show{1, 2, 3, w}
