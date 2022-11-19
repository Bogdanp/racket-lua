#lang lua

local str = racket["bytes"]
local len = racket["bytes-length"]
local ref = racket["bytes-ref"]
local subbytes = racket.subbytes

local string = {}

local function indices(s, i, j)
    local len = #s
    if i < 1 then
        i = 1 + (i % len)
    end
    j = j or -1
    if j < 1 then
        j = 1 + (j % len)
    end
    return i, j
end

function string.byte(s, i, j)
    if type(s) ~= "string" then
        error("string.sub: bad argument #1, expected a string", s)
    end
    local i, j = indices(s, i or 1, j or i or 1)
    if i > j then
        return nil
    end
    local function impl(i)
        if i <= j then
            return ref(s, i - 1), impl(i + 1)
        end
    end
    return impl(i)
end

function string.char(...)
    return str(...)
end

function string.len(s)
    return len(s)
end

function string.sub(s, i, j)
    if type(s) ~= "string" then
        error("string.sub: bad argument #1, expected a string", s)
    end
    if type(i) ~= "number" then
        error("string.sub: bad argument #2, expected a number", i)
    end
    local i, j = indices(s, i, j or -1)
    if i > j then
        return ""
    end
    return subbytes(s, i - 1, j)
end

return string
