#lang racket/base

(require "adjust.rkt"
         "error.rkt"
         "nil.rkt"
         "string.rkt"
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
     (if (procedure?* __len)
         (lua:adjust* (λ () (__len v)))
         (table-length v))]
    [else
     (lua:error (format "#: expected a string or a table, received ~a" (lua:tostring v)))]))


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
     (lua:error (format "rawlen: expected a string or a table, received ~a" (lua:tostring v)))]))
