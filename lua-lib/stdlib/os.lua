#lang lua

local current_seconds = racket["current-seconds"]
local current_process_millis = racket["current-process-milliseconds"]

local os = {}

function os.clock()
    return current_process_millis()
end

function os.time(t)
    if t ~= nil then
        error("os.time: table argument not implemented")
    end
    return current_seconds()
end

return os
