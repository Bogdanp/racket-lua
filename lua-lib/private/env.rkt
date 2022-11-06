#lang racket/base

(require "print.rkt"
         "table.rkt")

(provide
 current-global-environment
 make-initial-environment)

(define current-global-environment
  (make-parameter (make-table)))

(define (make-initial-environment)
  (define global (make-table))
  (begin0 global
    (table-set! global #"print" lua:print)))
