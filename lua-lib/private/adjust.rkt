#lang racket/base

(require "nil.rkt")

(provide
 lua:adjust*
 lua:adjust-va*)

(define (lua:adjust* proc)
  (call-with-values proc (λ vals (if (null? vals) nil (car vals)))))

(define (lua:adjust-va* proc)
  (call-with-values proc list))
