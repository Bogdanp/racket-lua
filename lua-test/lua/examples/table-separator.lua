#lang lua

local function print_table(t)
    print(string.format("== start %s ==", tostring(t)))
    for k, v in pairs(t) do
        print(k, v)
    end
    print("== end ==")
end

print_table({})
print_table({1,})
print_table({1,2,})
print_table({1;})
print_table({1;2;})
print_table({a = 1; b = 2;})
