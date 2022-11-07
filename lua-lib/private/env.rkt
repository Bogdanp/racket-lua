#lang racket/base

(require "error.rkt"
         "string.rkt"
         "table.rkt")

(provide
 make-initial-environment
 current-global-environment)

(define (make-initial-environment)
  (make-table
   ;; error
   `(#"error" . ,lua:error)
   `(#"pcall" . ,lua:pcall)

   ;; string
   `(#"print" . ,lua:print)
   `(#"tostring" . ,lua:tostring)

   ;; table
   `(#"getmetatable" . ,lua:getmetatable)
   `(#"setmetatable" . ,lua:setmetatable)))

(define current-global-environment
  (make-parameter (make-initial-environment)))
