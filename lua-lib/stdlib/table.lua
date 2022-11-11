#lang lua

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
    return table.unpack(args, index)
end

function table.pack(v, ...)
    if v == nil then
        return {}
    end
    local args = {v, ...}
    return {n = #args, table.unpack(args)}
end

function table.unpack(t, i, j)
    local function go(i, j)
        if i <= j then
            return t[i], go(i + 1, j)
        end
    end
    return go(i or 1, j or #t)
end

return table
