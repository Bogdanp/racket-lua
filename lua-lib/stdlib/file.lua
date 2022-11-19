#lang lua

local flush_output = racket["flush-output"]
local port_to_bytes = racket.lib("racket/port", "port->bytes")
local port_closed_p = racket["port-closed?"]
local close_input_port = racket["close-input-port"]
local close_output_port = racket["close-output-port"]
local eof = racket["eof"]
local eof_p = racket["eof-object?"]
local read_bytes = racket["read-bytes"]
local read_bytes_line = racket["read-bytes-line"]
local write_bytes = racket["write-bytes"]
local file_position = racket["file-position"]
local file_stream_buffer_mode = racket["file-stream-buffer-mode"]
local bytes_to_symbol = racket.compose1(racket["string->symbol"], racket["bytes->string/utf-8"])

local buf_syms = {
    no   = bytes_to_symbol("none"),
    line = bytes_to_symbol("line"),
    full = bytes_to_symbol("block")
}

local file = {}
file.__name = "file"
file.__index = file
function file.new(path, inp, out)
    if inp and out then
        error("file.new: either inp or out args must be provided, but not both")
    end
    local o = {
        path = path,
        _inp = inp,
        _out = out
    }
    setmetatable(o, file)
    return o
end

function file:close()
    if self._inp then
        close_input_port(self._inp)
    elseif self._out then
        close_output_port(self._out)
    end
end

function file:isclosed()
    return port_closed_p(self._inp or self._out)
end

function file:flush()
    if not self._out then
        error("file:flush: not writable")
    end
    flush_output(self._out)
end

function file:read(...)
    if not self._inp then
        error("file:read: not readable")
    elseif self:isclosed() then
        error("file:read: closed")
    end
    local function go(fmt, ...)
        if fmt ~= nil then
            local res
            if fmt == "a" or fmt == "*all" then
                res = port_to_bytes(self._inp)
            elseif fmt == "l" then
                res = read_bytes_line(self._inp)
            elseif type(fmt) == "number" then
                res = read_bytes(fmt, self._inp)
            else
                error("file:read: invalid format")
            end
            if eof_p(res) then
                return nil
            end
            return res, go(...)
        end
    end
    return go(...)
end

function file:write(...)
    if not self._out then
        error("file:write: not writable")
    elseif self:isclosed() then
        error("file:write: closed")
    end
    local port = self._out
    for _, arg in ipairs({...}) do
        write_bytes(tostring(arg), port)
    end
end

function file:seek(whence, offset)
    whence = whence or "curr"
    offset = offset or 0
    local port = self._inp or self._out
    if whence == "curr" then
        return file_position(port) + offset
    elseif whence == "set" then
        file_position(port, offset)
        return offset
    elseif whence == "end" then
        file_position(port, eof)
        local pos = file_position(port) - offset
        file_position(port, pos)
        return pos
    else
        error("file:seek: whence must be 'curr', 'set' or 'end'", whence)
    end
end

function file:lines(...)
    local fmt = {...}
    if #fmt == 0 then
        fmt = {"l"}
    end
    return function()
        return self:read(table.unpack(fmt))
    end
end

function file:setvbuf(mode, size)
    if size ~= nil then
        error("file:setvbuf: size argument not supported")
    end
    local port = self._inp or self._out
    local modeval = buf_syms[mode]
    if modeval == nil then
        error("file:setvbuf: invalid mode", mode)
    end
    file_stream_buffer_mode(port, modeval)
end

return file
