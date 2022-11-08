#lang lua

Callable = {}
function Callable:__call(...)
    print(self.name .. " called with:", ...)
end

SomeCallable = {name = "SomeCallable"}
setmetatable(SomeCallable, Callable)
SomeCallable(1, 2, "a", "b")
