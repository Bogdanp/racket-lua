#lang lua

-- TODO: Implement utf8.offset and utf8.charpattern.
-- For charpattern, we need to improve the lexer to support character
-- escapes in strings.

local str = racket["bytes"]
local len = racket["bytes-length"]
local ref = racket["bytes-ref"]

local utf8 = {}

local function check(who, v, argn, typ)
    typ = typ or "string"
    if type(v) ~= typ then
        argn = argn or 1
        error("utf8." .. who .. ": bad argument #" .. tostring(argn) .. "; expected a " .. typ, v)
    end
end

local function boundscheck(who, n, argn, pos)
    if pos < 1 or pos > n then
        error("utf8." .. who .. ": bad argument #" .. tostring(argn) .. "; out of bounds")
    end
end

local function decode(s, pos, who)
    local byte1 = ref(s, pos - 1)

    if byte1 <= 0x7F then
        return byte1, 1
    elseif byte1 >= 0xC2 and byte1 <= 0xDF then
        local byte2 = ref(s, pos)
        return (byte1 - 0xC0) * 0x40 + (byte2 - 0x80), 2
    elseif byte1 >= 0xE0 and byte1 <= 0xEF then
        local byte2 = ref(s, pos)
        local byte3 = ref(s, pos + 1)
        return (byte1 - 0xE0) * 0x1000 + (byte2 - 0x80) * 0x40 + (byte3 - 0x80), 3
    elseif byte1 >= 0xF0 and byte1 <= 0xF4 then
        local byte2 = ref(s, pos)
        local byte3 = ref(s, pos + 1)
        local byte4 = ref(s, pos + 2)
        return (byte1 - 0xF0) * 0x40000 + (byte2 - 0x80) * 0x1000 + (byte3 - 0x80) * 0x40 + (byte4 - 0x80), 4
    else
        error("utf8." .. who .. ": invalid UTF-8 code")
    end
end

function utf8.char(...)
    local args = {...}
    local n = #args
    if n == 0 then
        return ""
    end

    local bytes = {}
    local pos = 1
    for i = 1, n do
        local c = args[i]
        check("char", c, i, "number")
        c = math.floor(c)

        if c < 0 or c > 0x10FFFF then
            error("utf8.char: bad argument #" .. tostring(i) .. "; value out of range")
        end

        if c <= 0x7F then
            bytes[pos] = c
            pos = pos + 1
        elseif c <= 0x7FF then
            bytes[pos] = 0xC0 + c // 0x40
            bytes[pos + 1] = 0x80 + c % 0x40
            pos = pos + 2
        elseif c <= 0xFFFF then
            bytes[pos] = 0xE0 + c // 0x1000
            bytes[pos + 1] = 0x80 + c // 0x40 % 0x40
            bytes[pos + 2] = 0x80 + c % 0x40
            pos = pos + 3
        else
            bytes[pos] = 0xF0 + c // 0x40000
            bytes[pos + 1] = 0x80 + c // 0x1000 % 0x40
            bytes[pos + 2] = 0x80 + c // 0x40 % 0x40
            bytes[pos + 3] = 0x80 + c % 0x40
            pos = pos + 4
        end
    end
    return str(table.unpack(bytes))
end

function utf8.codes(s)
    check("codes", s)
    local n = #s
    local nlast = 0
    local function iter(s, pos)
        pos = pos + nlast
        if pos > n then
            return nil
        end

        local codepoint, nbytes = decode(s, pos, "codes")
        nlast = nbytes
        return pos, codepoint
    end
    return iter, s, 1
end

function utf8.codepoint(s, i, j)
    check("codepoint", s)
    local n = #s
    if n == 0 then
        return nil
    end
    i = i or 1
    j = j or i
    boundscheck("codepoint", n, 2, i)
    boundscheck("codepoint", n, 3, j)
    local results = {}
    local pos = i
    while pos <= j and pos <= n do
        local codepoint, nbytes = decode(s, pos, "codepoint")
        table.insert(results, codepoint)
        pos = pos + nbytes
    end
    return table.unpack(results)
end

function utf8.len(s, i, j)
    check("len", s)
    local n = #s
    if n == 0 then
        return 0
    end
    i = i or 1
    j = j or n
    boundscheck("len", n, 2, i)
    boundscheck("len", n, 3, j)
    local count = 0
    local pos = i
    while pos <= j and pos <= n do
        local ok, _, nbytes = pcall(decode, s, pos, "len")
        if not ok then
            return nil, pos
        end
        count = count + 1
        pos = pos + nbytes
    end
    return count
end

function utf8.offset(s, n, i)
    error"utf8.offset: not implemented"
end

return utf8
