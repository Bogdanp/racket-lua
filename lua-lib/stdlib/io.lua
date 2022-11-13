#lang lua

local bytes_to_path = racket["bytes->path"]
local open_input_file = racket["open-input-file"]
local open_output_file = racket["open-output-file"]
local get_current_input_port = racket["current-input-port"]
local get_current_output_port = racket["current-output-port"]
local get_current_error_port = racket["current-error-port"]

local STDIN, STDOUT, STDERR = 0, 1, 2

local io = {
    stdin  = STDIN,
    stdout = STDOUT,
    stderr = STDERR
}

function io.open(filename, mode)
    if type(filename) ~= "string" then
        error("io.open: expected a string filename", filename)
    end
    mode = mode or "r"
    path = bytes_to_path(filename)
    if mode == "r" or mode == "rb" then
        return file.new(filename, open_input_file(path))
    elseif mode == "w" or mode == "wb" then
        return file.new(filename, nil, open_output_file(path))
    else
        error("io.open: unsupported mode", mode)
    end
end

function io.close(f)
    if f then
        f:close()
    else
        io.output():close()
    end
end

function io.input(name)
    if not name or name == STDIN then
        return file.new("<stdin>", get_current_input_port())
    end
    return io.open(name, "r")
end

function io.output(name)
    if not name or name == STDOUT then
        return file.new("<stdout>", nil, get_current_output_port())
    elseif name == STDERR then
        return file.new("<stderr>", nil, get_current_error_port())
    end
    return io.open(name, "w")
end

function io.tmpfile()
    return io.output(os.tmpname())
end

function io.type(f)
    if getmetatable(f) == file then
        if f.isclosed() then
            return "closed file"
        end
        return "file"
    end
end

function io.write(...)
    io.output():write(...)
end

return io
