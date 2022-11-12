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
 table-ht
 table-length
 table-meta-ref
 table-ref
 lua:rawget
 table-set!
 lua:rawset
 table-keys
 table-values

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
  (for ([arg (in-list args)])
    (match arg
      [(and `(,k . ,(and (not (? list?)) v)))
       (hash-set! ht k v)]
      [_
       (unless (nil? arg)
         (hash-set! ht index arg))
       (set! index (add1 index))]))
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
  (cond
    [(table? t)
     (hash-ref (table-ht t) k default-proc)]
    [(nil? t)
     (raise-lua-error #f (format "attempt to index nil~n  index: ~a" (lua:tostring k)))]
    [(number? t)
     (raise-lua-error #f (format "attempt to index a number~n  value: ~a~n  index: ~a" t (lua:tostring k)))]
    [(boolean? t)
     (raise-lua-error #f (format "attempt to index a bool~n  index: ~a" (lua:tostring k)))]
    [else
     nil]))

(define (lua:rawget t k)
  (unless (table? t)
    (raise-lua-error #f (format "rawget: not a table: ~a" (lua:tostring k))))
  (hash-ref (table-ht t) k nil))

(define (table-set! t k v)
  (define dunder-value
    (table-meta-ref t #"__newindex"))
  (cond
    [(table? dunder-value)
     (table-set! dunder-value k v)]
    [(procedure? dunder-value)
     (dunder-value t k v)]
    [(nil? v)
     (hash-remove! (table-ht t) k)]
    [else
     (hash-set! (table-ht t) k v)]))

(define (lua:rawset t k v)
  (begin0 t
    (if (nil? v)
        (hash-remove! (table-ht t) k)
        (hash-set! (table-ht t) k v))))

(define (table-keys t)
  (hash-keys (table-ht t)))

(define (table-values t)
  (hash-keys (table-ht t)))

(define (lua:getmetatable t . _)
  (define meta
    (if (table? t)
        (table-meta t)
        nil))
  (cond
    [(nil? meta) nil]
    [else (table-ref meta #"__metatable" (λ () meta))]))

(define (lua:setmetatable t meta . _)
  (unless (table? meta)
    (raise-argument-error 'setmetatable "a table" 1 t meta))
  (begin0 t
    (set-table-meta! t meta)))
