#lang lua

local exit = racket.exit
local getenv = racket.getenv
local delete_file = racket["delete-file"]
local make_tmpfile = racket.lib("racket/file", "make-temporary-file")
local bytes_to_path = racket["bytes->path"]
local path_to_bytes = racket["path->bytes"]
local bytes_to_string = racket["bytes->string/utf-8"]
local current_seconds = racket["current-seconds"]
local current_process_millis = racket["current-process-milliseconds"]
local find_seconds = racket.lib("racket/date", "find-seconds")

local os = {}

function os.clock()
    return current_process_millis()
end

function os.date()
    error("os.date: not implemented")
end

function os.difftime(t2, t1)
    return t2 - t1
end

function os.execute()
    error("os.execute: not supported")
end

function os.exit(code)
    exit(code)
end

function os.getenv(varname)
    return getenv(bytes_to_string(varname))
end

function os.remove(path)
    return pcall(function() delete_file(bytes_to_path(path)) end)
end

function os.rename(oldname, newname)
    local oldpath = bytes_to_path(oldname)
    local newpath = bytes_to_path(newname)
    return pcall(function() rename_file_or_directory(oldpath, newpath) end)
end

function os.setlocale()
    error("os.setlocale: not supported")
end

function os.time(t)
    if t ~= nil then
        if t.isdst ~= nil then
            error"os.time: the isdst field is not supported"
        end
        for k, v in pairs(t) do
            if v < 0 then
                error"os.time: negative fields are not supported"
            end
        end
        return find_seconds(
            t.sec or 0,
            t.min or 0,
            t.hour or 12,
            t.day or 1,
            t.month or 1,
            t.year or 1970
        )
    end
    return current_seconds()
end

function os.tmpname()
    local path = make_tmpfile(bytes_to_string("luatmp~a"))
    delete_file(path)
    return path_to_bytes(path)
end

return os
