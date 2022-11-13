#lang lua

local call_bytes = racket.lib("racket/port", "call-with-output-bytes")
local write_bytes = racket["write-bytes"]

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

function table.concat(t, sep, i, j)
    sep = sep or ""
    if type(sep) ~= "string" then
        error("table.concat: invalid separator value")
    end
    i = i or 1
    j = j or #t
    local function impl(out)
        for k = i, j do
            local v = t[k]
            if k > i then
                write_bytes(sep, out)
            end
            if type(v) == "string" then
                write_bytes(v, out)
            elseif type(v) == "number" then
                write_bytes(tostring(v), out)
            else
                error("table.concat: invalid table value", v)
            end
        end
    end
    return call_bytes(impl)
end

return table
