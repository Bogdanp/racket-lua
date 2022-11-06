#lang racket/base

(require "print.rkt"
         "table.rkt")

(provide
 make-initial-environment
 current-global-environment)

(define (make-initial-environment)
  (define global (make-table))
  (begin0 global
    (table-set! global #"print" lua:print)))

(define current-global-environment
  (make-parameter (make-initial-environment)))
