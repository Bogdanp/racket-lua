#lang racket/base

(require "realm.rkt")

(provide
 (struct-out exn:fail:lua)
 raise-lua-error)

(struct exn:fail:lua exn:fail (value level))

(define missing
  (string->uninterned-symbol "missing"))

(define (raise-lua-error who message [value missing] [level 1])
  (define adjusted-message (error-message->adjusted-string who lua-realm message lua-realm))
  (define adjusted-value (if (eq? value missing) (string->bytes/utf-8 adjusted-message) value))
  (raise (exn:fail:lua adjusted-message (current-continuation-marks) adjusted-value level)))
