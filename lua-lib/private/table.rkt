#lang racket/base

(require racket/match
         "nil.rkt")

(provide
 table?
 make-table
 table-length
 table-ref
 table-set!)

(struct table (ht [border #:mutable]) #:transparent)

(define (make-table . args)
  (define ht (make-hash))
  (define index 1)
  (define border 0)
  (let loop ([args args])
    (for ([arg (in-list args)])
      (match arg
        [(? list?)
         (loop arg)]
        [`(,k . ,v)
         (hash-set! ht k v)
         (when (and (integer? k) (> k border))
           (set! border k))]
        [_
         (hash-set! ht index arg)
         (when (> index border)
           (set! border index))
         (set! index (add1 index))])))
  (table ht border))

(define (table-length t)
  (table-border t))

(define (table-ref t v)
  (hash-ref (table-ht t) v nil))

(define (table-set! t k v)
  (hash-set! (table-ht t) k v)
  (when (and (integer? k) (> k (table-border t)))
    (set-table-border! t k)))
