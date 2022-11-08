#lang racket/base

(require racket/match
         "nil.rkt")

(provide
 table?
 make-table
 table-length
 table-meta-ref
 table-ref
 table-set!
 table-keys

 lua:getmetatable
 lua:setmetatable)

(struct table (ht)
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
  (table ht))

(define (table-length t)
  (apply max 0 (hash-keys (table-ht t))))

(define (table-meta-ref t k [default-proc (λ () nil)])
  (define res
    (nil~>
     (lua:getmetatable t)
     (table-ref k)))
  (cond
    [(nil? res) (default-proc)]
    [else res]))

(define (table-ref t k [default-proc
                         (lambda ()
                           (define index (table-meta-ref t #"__index"))
                           (cond
                             [(table? index) (table-ref index k)]
                             [(procedure? index) (index t k)]
                             [else nil]))])
  (hash-ref (table-ht t) k default-proc))

(define (table-set! t k v)
  (if (nil? v)
      (hash-remove! (table-ht t) k)
      (hash-set! (table-ht t) k v)))

(define (table-keys t)
  (hash-keys (table-ht t)))

(define (lua:getmetatable t . _)
  (table-ref t #"__metatable" nil))

(define (lua:setmetatable t meta . _)
  (table-set! t #"__metatable" meta))
