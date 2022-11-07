#lang racket/base

(require racket/match
         "nil.rkt")

(provide
 table?
 make-table
 table-length
 table-ref
 table-set!

 lua:getmetatable
 lua:setmetatable)

(struct table ([meta #:mutable] ht)
  #:transparent)

(define (make-table . args)
  (define ht (make-hash))
  (define index 1)
  (let loop ([args args])
    (for ([arg (in-list args)])
      (match arg
        [(? list?)
         (loop arg)]
        [`(,k . ,v)
         (hash-set! ht k v)]
        [_
         (hash-set! ht index arg)
         (set! index (add1 index))])))
  (table nil ht))

(define (table-length t)
  (apply max 0 (hash-keys (table-ht t))))

(define (table-ref t v)
  (hash-ref (table-ht t) v (lambda ()
                             (nil~> (table-meta t)
                                    (table-ref #"__index")
                                    (table-ref v)))))

(define (table-set! t k v)
  (hash-set! (table-ht t) k v))

(define (lua:getmetatable t . _)
  (table-meta t))

(define (lua:setmetatable t meta . _)
  (set-table-meta! t meta))
