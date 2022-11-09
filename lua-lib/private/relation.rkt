#lang racket/base

(require "table.rkt")

;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.lua.org/manual/5.4/manual.html#3.4.4

(provide
 lua:==
 lua:~=)

(define (lua:== a b)
  (or (lua:rawequal a b)
      (and (table? a)
           (table? b)
           (let ([lhs-eq (table-meta-ref a #"__eq")]
                 [rhs-eq (table-meta-ref a #"__eq")])
             (cond
               [(procedure? lhs-eq) (and (lhs-eq a b) #t)]
               [(procedure? rhs-eq) (and (rhs-eq b a) #t)]
               [else #f])))))

(define (lua:~= a b)
  (not (lua:== a b)))


;; procedures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 lua:rawequal)

(define (lua:rawequal a b . _)
  (equal? a b))
