#lang racket/base

(require "string.rkt"
         "table.rkt")

(provide
 make-initial-environment
 current-global-environment)

(define (make-initial-environment)
  (make-table
   `(#"print" . ,lua:print)
   `(#"tostring" . ,lua:tostring)))

(define current-global-environment
  (make-parameter (make-initial-environment)))
