#lang racket/base

(require "exn.rkt"
         "nil.rkt"
         "string.rkt")

(provide
 lua:pcall
 lua:error
 lua:assert)

(define (lua:pcall proc . args)
  (with-handlers ([exn:fail:lua? (λ (e) (values #f (exn:fail:lua-value e)))]
                  [exn:fail? (λ (e) (values #f (exn-message e)))])
    (call-with-values
     (lambda ()
       (apply proc args))
     (lambda results
       (apply values #t results)))))

(define (lua:error v [level 1] . _)
  (define message (bytes->string/utf-8 (lua:tostring v)))
  (raise-lua-error 'error message v level))

(define (lua:assert v [message #"assertion failed!"] . _)
  (when (falsy? v)
    (lua:error message)))
