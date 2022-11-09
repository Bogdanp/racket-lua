#lang racket/base

(require "error.rkt"
         "iter.rkt"
         "length.rkt"
         "relation.rkt"
         "string.rkt"
         "table.rkt"
         "type.rkt")

;; global environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.lua.org/manual/5.4/manual.html#2.2

(provide
 make-initial-environment
 current-global-environment)

(define (make-initial-environment)
  (define env
    (make-table
     `(#"_VERSION" . #"racket-lua 0.1")

     ;; equal
     `(#"rawequal" . ,lua:rawequal)

     ;; error
     `(#"assert" . ,lua:assert)
     `(#"error" . ,lua:error)
     `(#"pcall" . ,lua:pcall)

     ;; iter
     `(#"next" . ,lua:next)
     `(#"pairs" . ,lua:pairs)
     `(#"ipairs" . ,lua:ipairs)

     ;; length
     `(#"rawlen" . ,lua:rawlen)

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
