#lang lua

--[ not a long bracket comment

--[[
    long brackets comments
    have multiline support
--]]

--[=[
    [[ nesting ]]
--]=]

print("after comments")

print([[test]])
print([[
  multi-line
  test
]])
print([==[
  [[nesting]]
]==])
