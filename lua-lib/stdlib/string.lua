#lang lua

local alloc = racket["make-bytes"]
local str = racket["bytes"]
local len = racket["bytes-length"]
local ref = racket["bytes-ref"]
local set = racket["bytes-set!"]
local copy = racket["bytes-copy!"]
local sub = racket.subbytes
local downcase = racket["string-downcase"]
local upcase = racket["string-upcase"]
local to_str = racket["bytes->string/locale"]
local to_bytes = racket["string->bytes/locale"]

local string = {}
string.__index = string
local function check(who, v, argn, typ)
    typ = typ or "string"
    if type(v) ~= typ then
        argn = argn or 1
        error("string." .. who .. ": bad argument #" .. tostring(argn) .. "; expected a " .. typ, v)
    end
end

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

local function numbers(who, a, b)
    a = tonumber(a)
    if not a then
        error("string." .. who .. ": bad lhs; expected a numeric string")
    end
    b = tonumber(b)
    if not b then
        error("string." .. who .. ": bad rhs; expected a numeric string")
    end
    return a, b
end

function string.__add(a, b)
    a, b = numbers("__add", a, b)
    return a + b
end

function string.__mul(a, b)
    a, b = numbers("__mul", a, b)
    return a * b
end

function string.byte(s, i, j)
    check("byte", s)
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

function string.dump()
    error"string.dump: not implemented"
end

function string.find(s, pattern, init, plain)
    error"string.find: not implemented"
end

function string.format(fmt, ...)
    error"string.format: bootstrap failed"
end

function string.gmatch(s, pattern, init)
    error"string.gmatch: not implemented"
end

function string.gsub(s, pattern, repl, init)
    error"string.gsub: not implemented"
end

function string.len(s)
    return len(s)
end

function string.lower(s)
    check("lower", s)
    return to_bytes(downcase(to_str(s)))
end

function string.match(s, pattern, init)
    error"string.match: not implemented"
end

function string.pack()
    error"string.pack: not implemented"
end

function string.packsize()
    error"string.packsize: not implemented"
end

function string.rep(s, n, sep)
    check("rep", s)
    check("rep", n, 2, "number")
    if n < 1 then
        return ""
    end
    sep = sep or ""
    check("rep", sep, 3)
    local srclen = #s
    local seplen = #sep
    local step = srclen + seplen
    local len = step * n - seplen
    local dst = alloc(len)
    copy(dst, 0, s)
    for i = srclen, len - 1, step do
        copy(dst, i, sep)
        copy(dst, i + seplen, s)
    end
    return dst
end

function string.reverse(s)
    check("reverse", s)
    local len = #s
    if len == 0 then
        return ""
    end
    local dst = alloc(len)
    local j = 0
    for i = len-1, 0, -1 do
        set(dst, j, ref(s, i))
        j = j + 1
    end
    return dst
end

function string.sub(s, i, j)
    check("sub", s)
    check("sub", i, 1, "number")
    local i, j = indices(s, i, j or -1)
    if i > j then
        return ""
    end
    return sub(s, i - 1, j)
end

function string.unpack(s)
    error"string.unpack: not implemented"
end

function string.upper(s)
    check("upper", s)
    return to_bytes(upcase(to_str(s)))
end

return string
