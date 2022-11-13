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

function table.insert(t, pos, value)
    local len = #t
    if value == nil then
        value = pos
        pos = len + 1
    end
    if pos == len + 1 then
        t[pos] = value
        return nil
    end
    if pos < 1 or pos > len + 1 then
        error("table.insert: position out of bounds", pos)
    end
    for i = len + 1, pos + 1, -1 do
        t[i] = t[i-1]
    end
    t[pos] = value
end

function table.remove(t, pos)
    local len = #t
    pos = pos or len
    if pos == 0 and len == 0 then
        return nil
    end
    if pos < 1 or pos > len then
        error("table.remove: position out of bounds", pos)
    end
    local value = t[pos]
    for i = pos, len do
        t[i] = t[i+1]
    end
    return value
end

return table
