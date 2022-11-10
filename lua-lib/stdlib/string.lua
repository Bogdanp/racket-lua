#lang lua

local str = require_racket("bytes")
local len = require_racket("bytes-length")

local string = {}

function string.char(...)
    return str(...)
end

function string.len(s)
    return len(s)
end

return string
