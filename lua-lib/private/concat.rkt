#lang racket/base

(require "adjust.rkt"
         "error.rkt"
         "string.rkt"
         "table.rkt")

;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.lua.org/manual/5.4/manual.html#3.4.6

(provide
 lua:..)

(define (lua:.. a b)
  (cond
    [(and (bytes? a) (bytes? b))
     (bytes-append a b)]
    [(and (bytes? a) (number? b))
     (bytes-append a (lua:tostring b))]
    [(and (number? a) (bytes? b))
     (bytes-append (lua:tostring a) b)]
    [(and (number? a) (number? b))
     (bytes-append (lua:tostring a) (lua:tostring b))]
    [else
     (define lhs-dunder-proc (and (table? a) (table-meta-ref a #"__concat")))
     (define rhs-dunder-proc (and (table? b) (table-meta-ref b #"__concat")))
     (cond
       [(procedure? lhs-dunder-proc)
        (lua:adjust* (λ () (lhs-dunder-proc a b)))]
       [(procedure? rhs-dunder-proc)
        (lua:adjust* (λ () (rhs-dunder-proc b a)))]
       [else
        (lua:error "..: cannot concatenate ~a and ~a"
                   (lua:tostring a)
                   (lua:tostring b))])]))
