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
local seconds_to_date = racket["seconds->date"]
local date_second = racket["date-second"]
local date_minute = racket["date-minute"]
local date_hour = racket["date-hour"]
local date_day = racket["date-day"]
local date_month = racket["date-month"]
local date_year = racket["date-year"]
local date_week_day = racket["date-week-day"]
local date_year_day = racket["date-year-day"]
local date_is_dst = racket["date-dst?"]
local date_to_string = racket.lib("racket/date", "date->string")

local os = {}

function os.clock()
    return current_process_millis()
end

function os.date(format, time)
    local isutc = false
    if format and format:byte(1) == 33 then
        isutc = true
        format = format:sub(2)
    end
    time = time or os.time()
    local date = seconds_to_date(time, isutc)
    if format == "*t" then
        return {
            year  = date_year(date),
            month = date_month(date),
            day   = date_day(date),
            hour  = date_hour(date),
            min   = date_minute(date),
            sec   = date_second(date),
            wday  = date_week_day(date),
            yday  = date_year_day(date),
            isdst = date_is_dst(date)
        }
    elseif format == "" or not format then
        return date_to_string(date, true)
    else
        error"os.date: '[!]' and '[!]*t' are the only formats currently supported"
    end
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
            if k ~= "isutc" and v < 0 then
                error"os.time: negative fields are not supported"
            end
        end
        local localtime = true
        if t.isutc ~= nil and t.isutc then
            localtime = false
        end
        return find_seconds(
            t.sec   or 0,
            t.min   or 0,
            t.hour  or 12,
            t.day   or 1,
            t.month or 1,
            t.year  or 1970,
            localtime
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
