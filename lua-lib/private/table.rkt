#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/lazy-require
         racket/match
         "exn.rkt"
         "nil.rkt")

(lazy-require
 ["string.rkt" (current-string-metatable lua:tostring)]
 ["relation.rkt" (lua:<)])

(provide
 table?
 make-table
 in-table
 table-ht
 table-length
 table-meta-ref
 table-ref
 lua:rawget
 table-set!
 lua:rawset
 table-keys
 table-values
 table-sort!

 lua:getmetatable
 lua:setmetatable)

;; hi: The largest inserted integer index. Serves as the upper bound
;; for finding borders in sequences.
(struct table ([meta #:mutable] [hi #:mutable] ht)
  #:transparent
  #:property prop:procedure
  (lambda (self . args)
    (define dunder-call
      (table-meta-ref self #"__call"))
    (cond
      [(procedure? dunder-call)
       (apply dunder-call self args)]
      [else
       (raise-lua-error #f (format "table ~a is not callable" (lua:tostring self)))])))

(define (make-table . args)
  (define t (table nil 0 (make-hash)))
  (begin0 t
    (for/fold ([index 1])
              ([arg (in-list args)])
      (match arg
        [(? nil?)
         (add1 index)]
        [(cons k (and (not (? pair?)) v))
         (unless (nil? v)
           (lua:rawset t k v))
         index]
        [_
         (lua:rawset t index arg)
         (add1 index)]))))

(define (table-length t)
  (define ht (table-ht t))
  (define hi (table-hi t))
  (cond
    [(hash-empty? ht) 0]
    [(and (hash-has-key? ht hi)
          (not (hash-has-key? ht (add1 hi))))
     (table-hi t)]
    [else
     (let loop ([lo 0]
                [hi hi])
       (define i
         (max 1 (quotient (+ lo hi) 2)))
       (if (hash-has-key? ht i)
           (if (hash-has-key? ht (add1 i))
               (loop (add1 i) hi)
               i)
           (if (= i 1)
               0
               (loop lo (sub1 i)))))]))

(define (table-meta-ref t k [default-proc (λ () nil)])
  (define res
    (nil~>
     (lua:getmetatable t)
     (table-ref k)))
  (if (nil? res)
      (default-proc)
      res))

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
     (define res
       (lua:rawget t k))
     (if (nil? res)
         (default-proc)
         res)]
    [(nil? t)
     (raise-lua-error #f (format "attempt to index nil~n  index: ~a" (lua:tostring k)))]
    [(number? t)
     (raise-lua-error #f (format "attempt to index a number~n  value: ~a~n  index: ~a" t (lua:tostring k)))]
    [(boolean? t)
     (raise-lua-error #f (format "attempt to index a bool~n  index: ~a" (lua:tostring k)))]
    [(bytes? t)
     (table-ref (current-string-metatable) k default-proc)]
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
    [else
     (lua:rawset t k v)]))

(define (lua:rawset t k v)
  (cond
    [(nil? k)
     (raise-lua-error #f "table index is nil")]
    [(nil? v)
     (hash-remove! (table-ht t) k)]
    [else
     (when (and (integer? k) (> k (table-hi t)))
       (set-table-hi! t k))
     (hash-set! (table-ht t) k v)]))

(define (table-keys t)
  (hash-keys (table-ht t)))

(define (table-values t)
  (hash-values (table-ht t)))

(define (lua:getmetatable t . _)
  (define meta
    (cond
      [(bytes? t)
       (current-string-metatable)]
      [(table? t)
       (table-meta t)]
      [else
       nil]))
  (cond
    [(nil? meta) nil]
    [else (table-ref meta #"__metatable" (λ () meta))]))

(define (lua:setmetatable t meta . _)
  (unless (table? meta)
    (raise-argument-error 'setmetatable "a table" 1 t meta))
  (begin0 t
    (set-table-meta! t meta)))

(define (table-sort! t [cmp lua:<])
  (define ht (table-ht t))
  (define vs
    (sort
     (for/list ([idx (in-naturals 1)]
                [v (in-table t)])
       (begin0 v
         (hash-remove! ht idx)))
     cmp))
  (for ([i (in-naturals 1)]
        [v (in-list vs)])
    (hash-set! (table-ht t) i v)))


;; sequence ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-sequence-syntax in-table
  (lambda () #'in-table*)
  (lambda (stx)
    (syntax-parse stx
      [[(v) (_ table)]
       #'[(v)
          (:do-in
           ([(t) table])
           (check-table t)
           ([i 1]
            [n (table-length t)])
           (<= i n)
           ([(j) (add1 i)]
            [(v) (table-ref t i)])
           #t
           #t
           [j n])]]
      [_ #f])))

(define (in-table* t)
  (for/list ([v (in-table t)]) v))

(define (check-table t)
  (unless (table? t)
    (raise-argument-error 'in-table "table?" t)))
