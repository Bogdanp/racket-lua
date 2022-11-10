#lang racket/base

(require "nil.rkt"
         "realm.rkt")

(provide
 (struct-out exn:fail:lua)
 raise-lua-error)

(struct exn:fail:lua exn:fail (value level))

(define (raise-lua-error who message [value nil] [level 1])
  (define adjusted-message (error-message->adjusted-string who lua-realm message lua-realm))
  (raise (exn:fail:lua adjusted-message (current-continuation-marks) value level)))
