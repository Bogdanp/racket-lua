#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         "adjust.rkt"
         "error.rkt"
         "nil.rkt"
         "string.rkt"
         "table.rkt")

;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.lua.org/manual/5.4/manual.html#3.4.4

(provide
 lua:==
 lua:~=)

(define (lua:== a b)
  (or (equal? a b)
      (and (table? a)
           (table? b)
           (let ([lhs-eq (table-meta-ref a #"__eq")]
                 [rhs-eq (table-meta-ref a #"__eq")])
             (cond
               [(procedure?* lhs-eq) (and (lhs-eq a b) #t)]
               [(procedure?* rhs-eq) (and (rhs-eq b a) #t)]
               [else #f])))))

(define (lua:~= a b)
  (not (lua:== a b)))

(define ((make-binop-dunder-proc who dunder-name) a b)
  (cond
    [(and dunder-name
          (or (table? a)
              (table? b)))
     (define lhs-dunder-proc (and (table? a) (table-meta-ref a dunder-name)))
     (define rhs-dunder-proc (and (table? b) (table-meta-ref b dunder-name)))
     (cond
       [(procedure?* lhs-dunder-proc)
        (lua:adjust* (λ () (lhs-dunder-proc a b)))]
       [(procedure?* rhs-dunder-proc)
        (lua:adjust* (λ () (rhs-dunder-proc b a)))]
       [else
        (lua:error (format "~a: expected two numbers or two strings, received ~a and ~a"
                           who
                           (lua:tostring a)
                           (lua:tostring b)))])]
    [else
     (lua:error (format "~a: expected two numbers or two strings, received ~a and ~a"
                        who
                        (lua:tostring a)
                        (lua:tostring b)))]))

(define-syntax (define-binop stx)
  (syntax-parse stx
    [(_ id:id who:string dunder-name:expr number-proc:expr string-proc:expr)
     #'(begin
         (provide id)
         (define dunder-proc
           (make-binop-dunder-proc 'who dunder-name))
         (define (id a b)
           (cond
             [(and (number? a)
                   (number? b))
              (number-proc a b)]
             [(and (bytes? a)
                   (bytes? b))
              (string-proc a b)]
             [else
              (dunder-proc a b)])))]))

(define-syntax-rule (define-binops [def ...] ...)
  (begin (define-binop def ...) ...))

(define (bytes<=? a b)
  (or (bytes=? a b) (bytes<? a b)))

(define (bytes>=? a b)
  (or (bytes=? a b) (bytes>? a b)))

(define-binops
  [lua:<  "<"  #"__lt" <  bytes<?]
  [lua:>  ">"  #f      >  bytes>?]
  [lua:<= "<=" #"__le" <= bytes<=?]
  [lua:>= ">=" #f      >= bytes>=?])


;; procedures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 lua:rawequal)

(define (lua:rawequal a b . _)
  (equal? a b))
