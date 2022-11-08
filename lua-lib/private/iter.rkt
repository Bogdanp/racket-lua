#lang racket/base

(require "error.rkt"
         "nil.rkt"
         "string.rkt"
         "table.rkt")

(provide
 lua:next
 lua:pairs
 lua:ipairs)

(define (lua:next t [given-idx nil] . _)
  (unless (table? t)
    (lua:error (format "next: expected a table, but received ~a" (lua:tostring t))))
  (define keys (list->vector (table-keys t)))
  (define len (vector-length keys))
  (define key
    (cond
      [(nil? given-idx)
       (cond
         [(zero? len) nil]
         [else (vector-ref keys 0)])]
      [else
       (define idx
         (for/first ([idx (in-naturals)]
                     [k (in-vector keys)]
                     #:when (equal? k given-idx))
           (add1 idx)))
       (unless idx
         (lua:error (format "next: invalid key ~a" (lua:tostring given-idx))))
       (cond
         [(< idx len) (vector-ref keys idx)]
         [else nil])]))
  (if (nil? key)
      (values nil nil)
      (values key (table-ref t key))))

(define (lua:pairs t . _)
  (define __pairs
    (table-meta-ref t #"__pairs"))
  (cond
    [(nil? __pairs)
     (values lua:next t nil)]
    [else
     (__pairs t)]))

(define (lua:ipairs t . _)
  (define (next s idx)
    (define v (table-ref s (add1 idx)))
    (if (nil? v)
        (values nil nil)
        (values (add1 idx) v)))
  (values next t 0))
