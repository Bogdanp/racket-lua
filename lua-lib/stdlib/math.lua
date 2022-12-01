#lang lua

local abs = racket.abs
local acos = racket.acos
local asin = racket.asin
local atan = racket.atan
local ceil = racket.ceiling
local cos = racket.cos
local deg = racket.lib("racket/math", "radians->degrees")
local exp = racket.exp
local floor = racket.floor
local log = racket.log
local pi = racket.lib("racket/math", "pi")
local rad = racket.lib("racket/math", "degrees->radians")
local random = racket.random
local random_seed = racket["random-seed"]
local sin = racket.sin
local sqrt = racket.sqrt
local tan = racket.tan
local number_p = racket["number?"]
local integer_p = racket["integer?"]

local math = {}
local function check(who, n, argn)
    if type(n) ~= "number" then
        argn = argn or 1
        error("math." .. who .. ": bad argument #" .. tostring(argn) .. "; not a number")
    end
end

function math.abs(n)
    check("abs", n)
    return abs(n)
end

function math.acos(n)
    check("acos", n)
    return acos(n)
end

function math.asin(n)
    check("acos", n)
    return asin(n)
end

function math.atan(y, x)
    check("atan", y)
    if x ~= nil then
        check("atan", x, 2)
        return atan(y, x)
    end
    return atan(y)
end

function math.ceil(n)
    check("ceil", n)
    return ceil(n)
end

function math.cos(n)
    check("cos", n)
    return cos(n)
end

function math.deg(n)
    check("deg", n)
    return deg(n)
end

function math.exp(n)
    check("exp", n)
    return exp(n)
end

function math.floor(n)
    check("floor", n)
    return floor(n)
end

function math.log(n, base)
    check("log", n)
    if base ~= nil then
        check("log", base, 2)
        return log(n, base)
    end
    return log(n)
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

math.pi = pi

function math.rad(n)
    check("rad", n)
    return rad(n)
end

function math.random(m, n)
    if m ~= nil then
        check("random", m)
        if n ~= nil then
            check("random", n, 2)
            return random(m, n)
        end
        return random(m)
    end
    return random()
end

function math.randomseed(x, y)
    x = x or os.time()
    y = y or 0
    random_seed((x ~ y) & 0x7FFFFFFF)
    return x, y
end

function math.sin(n)
    check("sin", n)
    return sin(n)
end

function math.sqrt(n)
    check("sqrt", n)
    return sqrt(n)
end

function math.tan(n)
    check("tan", n)
    return tan(n)
end

function math.tointeger(n)
    return tointeger(n)
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
