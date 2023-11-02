#lang racket/base

(require racket/lazy-require
         "exn.rkt"
         "nil.rkt")

(lazy-require
 ["string.rkt" (current-string-metatable lua:tostring)]
 ["relation.rkt" (lua:<)])

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
 table-sort!
 table->list

 lua:getmetatable
 lua:setmetatable)

(struct table ([meta #:mutable] ht)
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
  (define ht
    (make-hash
     (for/fold ([kvs null]
                [index 1]
                #:result kvs)
               ([arg (in-list args)])
       (cond
         [(nil? arg)
          (values kvs (add1 index))]
         [(and (pair? arg)
               (not (list? arg)))
          (if (nil? (cdr arg))
              (values kvs index)
              (values (cons arg kvs) index))]
         [else
          (values (cons (cons index arg) kvs) (add1 index))]))))
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
    [(nil? k)
     (raise-lua-error #f "table index is nil")]
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
    (cond
      [(nil? k)
       (raise-lua-error #f "table index is nil")]
      [(nil? v)
       (hash-remove! (table-ht t) k)]
      [else
       (hash-set! (table-ht t) k v)])))

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
  (define sorted-values
    (sort (table-values t) cmp))
  (hash-clear! (table-ht t))
  (for ([i (in-naturals 1)]
        [v (in-list sorted-values)])
    (hash-set! (table-ht t) i v)))

(define (table->list t)
  (for/list ([i (in-range 1 (add1 (table-length t)))])
    (table-ref t i)))
