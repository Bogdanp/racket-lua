#lang racket/base

(require "error.rkt"
         "nil.rkt"
         "string.rkt")

(provide
 lua:tonumber)

(define (lua:tonumber e [base nil] . _)
  (cond
    [(nil? e) nil]
    [(nil? base)
     (cond
       [(number? e) e]
       [(bytes? e) (string->number (bytes->string/utf-8 e))]
       [else (lua:error (format "tonumber: expected a number or a string, received ~a" (lua:tostring e)))])]
    [else
     (unless (bytes? e)
       (lua:error (format "tonumber: expected a string, received ~a" (lua:tostring e))))
     (unless (and (exact-integer? base)
                  (not (zero? base)))
       (lua:error (format "tonumber: expected a positive base, received ~a" (lua:tostring base))))
     (or (string->number (bytes->string/utf-8 e) base) nil)]))
