#lang racket/base

(require "nil.rkt")

(provide
 (struct-out exn:fail:lua)
 raise-lua-error)

(struct exn:fail:lua exn:fail (value level))

(define (raise-lua-error message [value nil] [level 1])
  (raise (exn:fail:lua message (current-continuation-marks) value level)))
