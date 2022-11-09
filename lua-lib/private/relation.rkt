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
           (or (let ([eq (table-meta-ref a #"__eq")])
                 (and (procedure? eq) (eq a b) #t))
               (let ([eq (table-meta-ref b #"__eq")])
                 (and (procedure? eq) (eq b a) #t))))))

(define (lua:~= a b)
  (not (lua:== a b)))


;; procedures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 lua:rawequal)

(define (lua:rawequal a b . _)
  (equal? a b))
