#lang lua

local app = racket.apply
local vals = racket.values
local cons = racket.cons
local list = racket.list
local reverse = racket.reverse

local table = {}

function table.select(index, ...)
    local args = {...}
    if index == "#" then
        return #args
    elseif type(index) ~= "number" then
        error("select: expected an integer index or '#'")
    end
    if index == 0 then
        error("select: 0 index")
    elseif index < 0 then
        index = 1 + (index % #args)
    end
    local t, j = {}, 1
    for i = index, #args do
        t[j], j = args[i], j + 1
    end
    return table.unpack(t)
end

function table.pack(v0, ...)
    if v0 == nil then
        return {}
    end
    local args = {v0, ...}
    return {n = #args, table.unpack(args)}
end

function table.unpack(t, i, j)
    i = i or 1
    j = j or #t
    local res = list()
    for i = i, j do
        res = cons(t[i], res)
    end
    return app(vals, reverse(res))
end

return table
