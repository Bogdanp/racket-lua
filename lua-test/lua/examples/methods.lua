#lang lua

-- https://www.lua.org/pil/16.html
Account = {balance = 0}
function Account:withdraw(amount)
    self.balance = self.balance - amount
end
function Account:deposit(amount)
    self.balance = self.balance + amount
end

local a = Account
a:withdraw(5)
print(a.balance)
a:deposit(25)
print(a.balance)

-- https://www.lua.org/pil/16.1.html
function Account:new(o)
    o = o or {}
    setmetatable(o, self)
    self.__index = self
    return o
end

local a = Account:new({balance = 0})
print(a)
a:deposit(50)
a:withdraw(10)
print(a.balance)

local b = Account:new({balance = 10})
b:withdraw(10)
print("b:", b.balance)
print("a:", a.balance)

-- https://www.lua.org/pil/16.2.html
SpecialAccount = Account:new()
function SpecialAccount:withdraw(amount)
    if amount - self.balance >= self:getLimit() then
        error("insufficient funds")
    end
    self.balance = self.balance - amount
end
function SpecialAccount:getLimit()
    return self.limit or 0
end

local c = SpecialAccount:new({ balance = 50, limit = 100})
print("c:", c)
print(c:getLimit())
c:withdraw(50)
print(c.balance)
local status, err = pcall(function() c:withdraw(150) end)
print(status, err)
