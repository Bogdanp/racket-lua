#lang racket/base

(require "error.rkt"
         "string.rkt"
         "table.rkt")

(provide
 make-initial-environment
 current-global-environment)

(define (make-initial-environment)
  (make-table
   `(#"error" . ,lua:error)
   `(#"pcall" . ,lua:pcall)
   `(#"print" . ,lua:print)
   `(#"tostring" . ,lua:tostring)))

(define current-global-environment
  (make-parameter (make-initial-environment)))
