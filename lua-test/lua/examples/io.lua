#lang lua

io.write("hello!\n")

local stdout = io.output()
stdout:write("hello", 42, "world!", "\n")

local stderr = io.output(io.stderr)
stderr:write("hello, errors!\n")
