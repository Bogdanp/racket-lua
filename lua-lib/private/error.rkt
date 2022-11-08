#lang racket/base

(require "nil.rkt"
         "string.rkt")

(provide
 lua:pcall
 lua:error
 lua:assert)

(struct exn:fail:lua exn:fail (value level))

(define (lua:pcall proc . args)
  (with-handlers ([exn:fail:lua? (λ (e) (values #f (exn:fail:lua-value e)))]
                  [exn:fail? (λ (e) (values #f (exn-message e)))])
    (values #t (apply proc args))))

(define (lua:error v [level 1] . _)
  (define message (format "error: ~a" (lua:tostring v)))
  (raise (exn:fail:lua message (current-continuation-marks) v level)))

(define (lua:assert v [message #"assertion failed!"] . _)
  (when (falsy? v)
    (lua:error message)))
