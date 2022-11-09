#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         "adjust.rkt"
         "error.rkt"
         "string.rkt"
         "table.rkt")

;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.lua.org/manual/5.4/manual.html#3.4.1

(provide
 lua:negate)

(define (lua:negate v)
  (cond
    [(number? v) (- v)]
    [else
     (define dunder-proc
       (and (table? v)
            (table-meta-ref v #"__unm")))
     (if (procedure? dunder-proc)
         (lua:adjust* (λ () (dunder-proc v)))
         (lua:error (format "-: expected a number, received ~a" (lua:tostring v))))]))

(define ((make-binop who dunder-name integer-proc real-proc [coerce-ints? #f]) a b)
  (cond
    [(and (not coerce-ints?)
          (exact-integer? a)
          (exact-integer? b))
     (integer-proc a b)]
    [(and (number? a)
          (number? b))
     (real-proc (exact->inexact a)
                (exact->inexact b))]
    [else
     (define lhs-dunder-proc (and (table? a) (table-meta-ref a dunder-name)))
     (define rhs-dunder-proc (and (table? b) (table-meta-ref b dunder-name)))
     (cond
       [(procedure? lhs-dunder-proc)
        (lua:adjust* (λ () (lhs-dunder-proc a b)))]
       [(procedure? rhs-dunder-proc)
        (lua:adjust* (λ () (rhs-dunder-proc b a)))]
       [else
        (lua:error (format "~a: expected two numbers, received ~a and ~a"
                           who
                           (lua:tostring a)
                           (lua:tostring b)))])]))

(define-syntax (define-binop stx)
  (syntax-parse stx
    [(_ id:id who:string dunder-name:bytes integer-proc:expr {~optional real-proc:expr})
     #'(begin
         (provide id)
         (define id (make-binop 'who dunder-name integer-proc {~? real-proc integer-proc})))]))

(define-syntax-rule (define-binops [def ...] ...)
  (begin (define-binop def ...) ...))

(define (div/real a b)
  (/ (exact->inexact a)
     (exact->inexact b)))

(define (modulo/real a b)
  (modulo
   (floor a)
   (floor b)))

(define (quotient/real a b)
  (floor (/ a b)))

(define-binops
  [lua:+  "+"  #"__add"  +]
  [lua:-  "-"  #"__sub"  -]
  [lua:*  "*"  #"__mul"  *]
  [lua:/  "/"  #"__div"  div/real /]
  [lua:// "//" #"__idiv" quotient quotient/real]
  [lua:%  "%"  #"__mod"  modulo modulo/real]
  [lua:^  "^"  #"__pow"  expt])


;; binary operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.lua.org/manual/5.4/manual.html#3.4.1

(provide
 lua:bnegate)

(define (lua:bnegate v)
  (cond
    [(exact-integer? v)
     (bitwise-not v)]
    [(integer? v)
     (bitwise-not (inexact->exact v))]
    [else
     (define dunder-proc
       (and (table? v)
            (table-meta-ref v #"__bnot")))
     (if (procedure? dunder-proc)
         (lua:adjust* (λ () (dunder-proc v)))
         (lua:error (format "~~: expected an integer, received ~a" (lua:tostring v))))]))

(define ((make-bit-binop who dunder-name proc) a b)
  (cond
    [(and (exact-integer? a) (exact-integer? b))
     (proc a b)]
    [(and (exact-integer? a) (integer? b))
     (proc a (inexact->exact b))]
    [(and (integer? a) (exact-integer? b))
     (proc (inexact->exact a) b)]
    [else
     (define lhs-dunder-proc (and (table? a) (table-meta-ref a dunder-name)))
     (define rhs-dunder-proc (and (table? b) (table-meta-ref b dunder-name)))
     (cond
       [(procedure? lhs-dunder-proc)
        (lua:adjust* (λ () (lhs-dunder-proc a b)))]
       [(procedure? rhs-dunder-proc)
        (lua:adjust* (λ () (rhs-dunder-proc b a)))]
       [else
        (lua:error (format "~a: expected two integers, received ~a and ~a"
                           who
                           (lua:tostring a)
                           (lua:tostring b)))])]))

(define-syntax (define-bit-binop stx)
  (syntax-parse stx
    [(_ id:id who:string dunder-name:bytes proc:expr)
     #'(begin
         (provide id)
         (define id (make-bit-binop 'who dunder-name proc)))]))

(define-syntax-rule (define-bit-binops [def ...] ...)
  (begin (define-bit-binop def ...) ...))

(define (shl a b) (arithmetic-shift a b))
(define (shr a b) (arithmetic-shift a (- b)))

(define-bit-binops
  [lua:&   "&"  #"__band" bitwise-and]
  [lua:bor "|"  #"__bor"  bitwise-ior]
  [lua:~   "~"  #"__bxor" bitwise-xor]
  [lua:<<  "<<" #"__shl"  shl]
  [lua:>>  ">>" #"__shr"  shr])
