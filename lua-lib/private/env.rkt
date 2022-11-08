#lang racket/base

(require "error.rkt"
         "string.rkt"
         "table.rkt"
         "type.rkt")

(provide
 make-initial-environment
 current-global-environment)

(define (make-initial-environment)
  (define env
    (make-table
     `(#"_VERSION" . #"racket-lua 0.1")

     ;; error
     `(#"assert" . ,lua:assert)
     `(#"error" . ,lua:error)
     `(#"pcall" . ,lua:pcall)

     ;; string
     `(#"print" . ,lua:print)
     `(#"tostring" . ,lua:tostring)

     ;; table
     `(#"getmetatable" . ,lua:getmetatable)
     `(#"setmetatable" . ,lua:setmetatable)

     ;; type
     `(#"type" . ,lua:type)))
  (begin0 env
    (table-set! env #"_G" env)))

(define current-global-environment
  (make-parameter (make-initial-environment)))
