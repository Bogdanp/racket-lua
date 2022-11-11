#lang lua

local Target = {}
local MetaWithTable = {}
MetaWithTable.__newindex = Target

local t0 = {}
setmetatable(t0, MetaWithTable)
t0.x = 42
print(t0.x)
print(Target.x)

local MetaWithProc = {}
function MetaWithProc:__newindex(k, v)
    print(k, "<-", v)
    rawset(self, k, v)
end

local t1 = {}
setmetatable(t1, MetaWithProc)
t1.foo = 42
print(t1.foo)
