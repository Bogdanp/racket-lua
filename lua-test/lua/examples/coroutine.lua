#lang lua

print(coroutine.running())
print(coroutine.isyieldable())
print(pcall(function() coroutine.close(coroutine.running()) end))

local gen = coroutine.create(function()
        for i = 1, 3 do
            print("yield", coroutine.yield(i))
        end
        return 4, 5
end)

print(gen)
print("status", coroutine.status(gen))
print("resume", coroutine.resume(gen))
print("status", coroutine.status(gen))

print("resume", coroutine.resume(gen, 'a', 'b'))
print("status", coroutine.status(gen))

print("resume", coroutine.resume(gen, 'b', 'c'))
print("status", coroutine.status(gen))

print("resume", coroutine.resume(gen, 'c', 'd'))
print("status", coroutine.status(gen))

print("resume", coroutine.resume(gen, 'c', 'd'))
print("close", coroutine.close(gen))

local function make_incr(amt)
    local i = 0
    return coroutine.wrap(function()
            while true do
                print("yield incr", coroutine.yield(i))
                i = i + amt
            end
    end)
end
local incr5 = make_incr(5)
print(incr5())
print(incr5(1))
print(incr5(2))
