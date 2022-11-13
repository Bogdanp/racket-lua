#lang lua

local callcc = racket["call/cc"]

local suspended, running, dead = 1, 2, 3
local statuses = {"suspended", "running", "dead"}
local thread = {}
thread.__name = "thread"
thread.__index = thread
function thread.new(proc, status)
    o = { k = proc, status = status or suspended }
    setmetatable(o, thread)
    return o
end

local coroutine = {}
local current = thread.new(nil, running)
local main = current

function coroutine.close(coro)
    if coro.status == suspended then
        error("coroutine.close: cannot close a running coroutine")
    end
    coro.status = "dead"
    coro.k = nil
    coro.yield = nil
    return true
end

function coroutine.create(proc)
    return thread.new(proc)
end

function coroutine.isyieldable(coro)
    coro = coro or current
    return coro.yield ~= nil
end

function coroutine.resume(coro, ...)
    if coro.status ~= suspended then
        return false, "coroutine.resume: cannot resume non-suspended coroutine"
    end

    local args = {...}
    local function impl(k)
        if current.status ~= dead then
            current.status = suspended
            current.k = k
        end
        current.yield = nil
        local old = current

        current = coro
        coro.status = running
        coro.yield = function(...)
            return coroutine.resume(old, true, ...)
        end

        local function resume()
            return table.pack(coro.k(table.unpack(args)))
        end
        local status, res = pcall(resume)
        coro.status = dead
        if status then
            coroutine.resume(old, true, table.unpack(res))
        else
            coroutine.resume(old, false, res)
        end
    end
    return callcc(impl)
end

function coroutine.running()
    return current, current == main
end

function coroutine.status(c)
    return statuses[c.status]
end

function coroutine.wrap(proc)
    local coro = coroutine.create(proc)
    local wrapped = function(...)
        return select(2, coroutine.resume(coro, ...))
    end
    return wrapped
end

function coroutine.yield(...)
    if not current.yield then
        error("coroutine.yield: attempt to yield from outside a coroutine")
    end
    return current.yield(...)
end

return coroutine
