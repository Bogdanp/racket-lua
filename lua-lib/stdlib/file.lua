#lang lua

local input_port_p = racket["input-port?"]
local output_port_p = racket["output-port?"]
local flush_output = racket["flush-output"]
local close_input_port = racket["close-input-port"]
local close_output_port = racket["close-output-port"]
local write_bytes = racket["write-bytes"]

local file = {}
file.__name = "file"
file.__index = file
function file._open(p)
    local o = {_port = p}
    setmetatable(o, file)
    return o
end

function file:write(...)
    if not output_port_p(self._port) then
        error("file:write: not writable")
    end
    local port = self._port
    for _, arg in ipairs({...}) do
        write_bytes(tostring(arg), port)
    end
end

function file:flush()
    if output_port_p(self._port) then
        flush_output(self._port)
    end
end

function file:close()
    if input_port_p(self._port) then
        close_input_port(self._port)
    else
        close_output_port(self._port)
    end
end

return file
