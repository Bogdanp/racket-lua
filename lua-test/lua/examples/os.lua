#lang lua

print(math.type(os.clock()))
print(math.type(os.time()))
print(os.time({ year = 2023, month = 5, day = 29, isutc = true }))
print(os.time({ year = 2023, month = 5, day = 29, hour = 5, minute = 10, second = 25, isutc = true }))
print(pcall(function() os.time({ year = 2023, month = 5, day = 29, minute = -15 }) end))
