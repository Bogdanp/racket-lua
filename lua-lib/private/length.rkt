#lang racket/base

(require "table.rkt")

(provide
 lua:length
 lua:rawlen)

(define (lua:length v)
  (cond
    [(bytes? v)
     (bytes-length v)]
    [(table? v)
     (define __len
       (table-meta-ref v #"__len"))
     (if (procedure? __len)
         (__len v)
         (table-length v))]
    [else
     (raise-argument-error "#" "a string or a table" v)]))

(define (lua:rawlen v . _)
  (cond
    [(bytes? v)
     (bytes-length v)]
    [(table? v)
     (table-length v)]
    [else
     (raise-argument-error "rawlen" "a string or a table" v)]))
