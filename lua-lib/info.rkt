#lang info

(define collection "lua")
(define version "0.1")
(define deps '("base"))
(define compile-include-files
  '("stdlib/coroutine.lua"
    "stdlib/file.lua"
    "stdlib/io.lua"
    "stdlib/math.lua"
    "stdlib/os.lua"
    "stdlib/racket.lua"
    "stdlib/string.lua"
    "stdlib/table.lua"))
