#lang info

(define license 'BSD-3-Clause)
(define collection "lua")
(define version "0.4")
(define deps '("base"
               "iso-printf-lib"
               "sandbox-lib"))
(define raco-commands '(("lua" (submod lua/cli main) "run lua scripts" #f)))
