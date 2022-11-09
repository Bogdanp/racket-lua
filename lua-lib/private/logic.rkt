#lang racket/base

(require "nil.rkt")

(provide
 lua:not
 lua:and
 lua:or)

;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.lua.org/manual/5.4/manual.html#3.4.5

(define (lua:not v)
  (if (nil? v) #t (not v)))

(define-syntax-rule (lua:and a b)
  (if (falsy? a) nil b))

(define-syntax-rule (lua:or a b)
  (if (falsy? a) b a))
