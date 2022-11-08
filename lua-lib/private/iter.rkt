#lang racket/base

(require "error.rkt"
         "nil.rkt"
         "string.rkt"
         "table.rkt")

(provide
 lua:next)

(define (lua:next t [start-idx nil] . _)
  (unless (table? t)
    (lua:error (format "next: expected a table, but received ~a" (lua:tostring t))))
  (define keys (list->vector (table-keys t)))
  (define idx
    (cond
      [(nil? start-idx) 1]
      [else
       (define matched-idx
         (for/first ([idx (in-naturals 1)]
                     [k (in-vector keys)]
                     #:when (equal? k start-idx))
           idx))
       (begin0 matched-idx
         (unless matched-idx
           (lua:error (format "next: invalid key ~a" (lua:tostring start-idx)))))]))
  (cond
    [(nil? idx)
     (values nil nil)]
    [(zero? (vector-length keys))
     (values nil nil)]
    [else
     (define key
       (vector-ref keys (sub1 idx)))
     (if (< idx (vector-length keys))
         (values key (vector-ref keys idx))
         (values key nil))]))
