#lang racket/base

(require racket/lazy-require
         racket/match
         "exn.rkt"
         "nil.rkt")

(lazy-require
 ["string.rkt" (lua:tostring)])

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

(struct table ([meta #:mutable] ht)
  #:transparent
  #:property prop:procedure
  (lambda (self . args)
    (define __call
      (table-meta-ref self #"__call"))
    (cond
      [(procedure? __call)
       (apply __call self args)]
      [else
       (error 'application "table ~a is not callable" (lua:tostring self))])))

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
  (define ints
    (filter integer? (hash-keys (table-ht t))))
  (apply max 0 ints))

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
                           (define index
                             (table-meta-ref t #"__index"))
                           (cond
                             [(table? index) (table-ref index k)]
                             [(procedure? index) (index t k)]
                             [else nil]))])
  (if (nil? t)
      (raise-lua-error #f (format "attempt to index a nil value~n  index: ~a" (lua:tostring k)))
      (hash-ref (table-ht t) k default-proc)))

(define (table-set! t k v)
  (if (nil? v)
      (hash-remove! (table-ht t) k)
      (hash-set! (table-ht t) k v)))

(define (table-keys t)
  (hash-keys (table-ht t)))

(define (lua:getmetatable t . _)
  (define meta
    (table-meta t))
  (cond
    [(nil? meta) nil]
    [else (table-ref meta #"__metatable" (λ () meta))]))

(define (lua:setmetatable t meta . _)
  (unless (table? meta)
    (raise-argument-error 'setmetatable "a table" 1 t meta))
  (begin0 t
    (set-table-meta! t meta)))
