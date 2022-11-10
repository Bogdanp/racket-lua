#lang lua

local str = racket["bytes"]
local len = racket["bytes-length"]

local string = {}

function string.char(...)
    return str(...)
end

function string.len(s)
    return len(s)
end

return string
