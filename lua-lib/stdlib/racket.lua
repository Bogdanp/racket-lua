#lang lua

local Racket = {}
local require = racket["#%require"]
function Racket:__index(id)
    return require(id)
end

local racket = {}
setmetatable(racket, Racket)
function racket.lib(mod, id)
    return require(id, mod)
end

local list = racket.list
local null = racket.null
local apply = racket.apply
local values = racket.values
local time_apply = racket["time-apply"]
function racket.time(proc, ...)
    local res, cpu, real, gc = time_apply(proc, list(...))
    print("cpu time:", cpu, "real time:", real, "gc:", gc)
    return apply(values, res)
end

return racket
