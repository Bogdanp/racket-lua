#lang lua

function closing_iter(proc)
    return function(s, c)
        if c < 1 then
            return 1
        end
    end, {}, 0, proc
end

for v in closing_iter(function() print("closed") end) do
    print(v)
end

pcall(function()
        for v in closing_iter(function() print("closed") end) do
            error("fail")
        end
end)
