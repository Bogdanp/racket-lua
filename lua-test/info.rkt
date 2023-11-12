#lang info

(define license 'BSD-3-Clause)
(define collections "tests")
(define deps '("base"
               "lua-lib"
               "rackcheck-lib"
               "rackunit-lib"))
(define build-deps '("rackunit-lib"))
(define implies '("lua-lib"))
