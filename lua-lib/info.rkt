#lang info

(define collection "lua")
(define version "0.1")
(define deps '("base" "sandbox-lib"))
(define raco-commands '(("lua" (submod lua/cli main) "run lua scripts" #f)))
