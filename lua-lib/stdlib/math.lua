#lang lua

local abs = racket.abs
local ceil = racket.ceiling
local floor = racket.floor
local sqrt = racket.sqrt
local number_p = racket["number?"]
local integer_p = racket["integer?"]

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

function math.sqrt(n)
    check("sqrt", n)
    return sqrt(n)
end

function math.type(n)
    if integer_p(n) then
        return "integer"
    elseif number_p(n) then
        return "float"
    end
    return nil
end

return math
