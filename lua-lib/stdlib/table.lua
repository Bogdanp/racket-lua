#lang lua

local app = require_racket("apply")
local vals = require_racket("values")
local cons = require_racket("cons")
local list = require_racket("list")
local reverse = require_racket("reverse")

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
