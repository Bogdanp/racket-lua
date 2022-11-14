#lang racket/base

(require "realm.rkt")

(provide
 (struct-out exn:fail:lua)
 raise-lua-error)

(struct exn:fail:lua exn:fail (value level))

(define (raise-lua-error who message [value (string->bytes/utf-8 message)] [level 1])
  (define adjusted-message (error-message->adjusted-string who lua-realm message lua-realm))
  (raise (exn:fail:lua adjusted-message (current-continuation-marks) value level)))
