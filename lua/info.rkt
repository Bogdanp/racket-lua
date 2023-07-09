#lang info

(define license 'BSD-3-Clause)
(define collection "lua")
(define deps '("base"
               "lua-lib"))
(define build-deps '("base"
                     "racket-doc"
                     "sandbox-lib"
                     "scribble-lib"))
(define implies '("lua-lib"))
(define scribblings '(("lua-manual.scrbl" () (language))))
