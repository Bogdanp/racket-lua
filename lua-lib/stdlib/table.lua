#lang lua

local app = racket.apply
local vals = racket.values
local cons = racket.cons
local list = racket.list
local reverse = racket.reverse

local table = {}

function table.unpack(t, i, j)
    i = i or 1
    j = j or #t
    local res = list()
    for i = i, j do
        res = cons(t[i], res)
    end
    return app(vals(reverse(res)))
end

return table
