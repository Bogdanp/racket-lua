#lang lua

print(pcall(function() idontexist(1, 2, 3) end))

local t = {}
function t.c()
    return d(42)
end

function a()
    return b()
end

function b()
    return t.c()
end

print(pcall(function() a() end))

function loop()
    local function go(n)
        if n > 1000 then
            t.c()
        else
            go(n + 1)
        end
    end
    go(1)
end

print(pcall(function() loop() end))

print(pcall(function() t:d() end))
