#lang lua

local exit = racket.exit
local delete_file = racket["delete-file"]
local make_tmpfile = racket.lib("racket/file", "make-temporary-file")
local bytes_to_path = racket["bytes->path"]
local path_to_bytes = racket["path->bytes"]
local bytes_to_string = racket["bytes->string/utf-8"]
local current_seconds = racket["current-seconds"]
local current_process_millis = racket["current-process-milliseconds"]

local os = {}

function os.clock()
    return current_process_millis()
end

function os.exit(code)
    exit(code)
end

function os.rename(oldname, newname)
    local oldpath = bytes_to_path(oldname)
    local newpath = bytes_to_path(newname)
    return pcall(function() rename_file_or_directory(oldpath, newpath) end)
end

function os.remove(path)
    return pcall(function() delete_file(bytes_to_path(path)) end)
end

function os.time(t)
    if t ~= nil then
        error("os.time: table argument not implemented")
    end
    return current_seconds()
end

function os.tmpname()
    local path = make_tmpfile(bytes_to_string("luatmp~a"))
    delete_file(path)
    return path_to_bytes(path)
end

return os
