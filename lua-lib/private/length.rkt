#lang racket/base

(require "adjust.rkt"
         "nil.rkt"
         "table.rkt")

;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.lua.org/manual/5.4/manual.html#3.4.7

(provide
 lua:length)

(define (lua:length v)
  (cond
    [(bytes? v)
     (bytes-length v)]
    [(table? v)
     (define __len
       (table-meta-ref v #"__len"))
     (if (procedure? __len)
         (lua:adjust* (λ () (__len v)))
         (table-length v))]
    [else
     (raise-argument-error "#" "a string or a table" v)]))


;; procedures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 lua:rawlen)

(define (lua:rawlen v . _)
  (cond
    [(bytes? v)
     (bytes-length v)]
    [(table? v)
     (table-length v)]
    [else
     (raise-argument-error "rawlen" "a string or a table" v)]))
