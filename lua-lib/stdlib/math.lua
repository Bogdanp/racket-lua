#lang lua

local abs = racket.abs
local ceil = racket.ceiling
local floor = racket.floor

local math = {}
local function check(who, n)
    if type(n) ~= "number" then
        error("math." .. who .. ": not a number")
    end
end

function math.abs(n)
    check("abs", n)
    return abs(n)
end

function math.ceil(n)
    check("ceil", n)
    return ceil(n)
end

function math.floor(n)
    check("floor", n)
    return floor(n)
end

function math.min(n, ...)
    if n == nil then
        error("math.min: bad argument #1 to min (value expected)")
    end
    local min = n
    for _, n in ipairs({...}) do
        if n < min then
            min = n
        end
    end
    return min
end

function math.max(n, ...)
    if n == nil then
        error("math.max: bad argument #1 to max (value expected)")
    end
    local max = n
    for _, n in ipairs({...}) do
        if max < n then
            max = n
        end
    end
    return max
end

return math
