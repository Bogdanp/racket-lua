#lang racket/base

(require "table.rkt")

(provide
 lua:rawequal
 lua:==
 lua:~=)

(define (lua:rawequal a b . _)
  (equal? a b))

(define (lua:== a b)
  (or (lua:rawequal a b)
      (and (table? a)
           (table? b)
           (or (let ([eq (table-meta-ref a #"__eq")])
                 (and (procedure? eq) (eq a b) #t))
               (let ([eq (table-meta-ref b #"__eq")])
                 (and (procedure? eq) (eq b a) #t))))))

(define (lua:~= a b)
  (not (lua:== a b)))
