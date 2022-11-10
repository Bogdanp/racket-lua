#lang lua

local get_current_output_port = racket["current-output-port"]
local get_current_error_port = racket["current-error-port"]

local STDOUT, STDERR = 1, 2

local io = {
    stdout = STDOUT,
    stderr = STDERR,

    output = function(name)
        if not name or name == STDOUT then
            return file._open(get_current_output_port())
        elseif name == STDERR then
            return file._open(get_current_error_port())
        else
            error("io.output: only responds to io.stdout and io.stderr")
        end
    end,

    write = function(...)
        io.output():write(...)
    end
}

return io
