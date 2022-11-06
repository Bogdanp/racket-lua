#lang racket/base

(require racket/match)

;; kernel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 #%app
 #%datum
 #%top
 (rename-out
  [#%plain-module-begin #%module-begin]
  [begin #%begin]
  [call/cc #%call/cc]
  [cond #%cond]
  [cons #%cons]
  [define #%define]
  [else #%else]
  [error #%error]
  [let #%let]
  [let/ec #%let/ec]
  [provide #%provide]
  [set! #%set!]
  [unless #%unless]
  [values #%values]
  [void #%void]
  [when #%when]))


;; basics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (rename-out
  [lua-nil nil]
  [lua-print print]))

(define lua-nil (gensym 'nil))

(define (lua-print v)
  (cond
    [(eq? v lua-nil)
     (displayln "nil")]
    [else
     (displayln v)]))


;; arithmetic operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.lua.org/manual/5.4/manual.html#3.4.1

(provide
 (rename-out
  [lua-unary-- #%unary-minus]
  [lua-+ +]
  [lua-- -]
  [lua-* *]
  [lua-/ /]
  [lua-// //]
  [lua-% %]
  [lua-pow ^]))

(define-syntax-rule (define-numeric-unop id who integer-proc real-proc)
  (define (id x)
    (cond
      [(integer? x) (integer-proc x)]
      [(real? x) (real-proc x)]
      [else (raise-argument-error 'who "expected a number" x)])))

(define-numeric-unop lua-unary-- '- - -)

(define-syntax-rule (define-numeric-binop id who integer-proc real-proc)
  (define (id x y)
    (cond
      [(and (exact-integer? x)
            (exact-integer? y))
       (integer-proc x y)]

      [(and (number? x)
            (number? y))
       (real-proc (exact->inexact x)
                  (exact->inexact y))]

      [else
       (raise-arguments-error 'who "expected two numbers" "x" x "y" y)])))

(define (lua-real-modulo x y)
  (modulo
   (floor x)
   (floor y)))

(define (lua-real-quotient x y)
  (floor (/ x y)))

(define-numeric-binop lua-+ + + +)
(define-numeric-binop lua-- - - -)
(define-numeric-binop lua-* * * *)
(define-numeric-binop lua-% % modulo lua-real-modulo)
(define-numeric-binop lua-// // quotient lua-real-quotient)

(define (lua-/ x y)
  (cond
    [(and (number? x)
          (number? y))
     (/ (exact->inexact x)
        (exact->inexact y))]

    [else
     (raise-arguments-error '/ "expected two numbers" "x" x "y" y)]))

(define lua-pow
  (procedure-rename expt 'pow))


;; relational operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.lua.org/manual/5.4/manual.html#3.4.4

(provide
 (rename-out
  [not #%unary-not]
  [lua-== ==]
  [lua-~= ~=]
  [lua-<  < ]
  [lua-<= <=]
  [lua->  > ]
  [lua->= >=]))

(define lua-==
  (procedure-rename equal? '==))
(define (lua-~= a b)
  (not (lua-== a b)))

(define-numeric-binop lua-<  <  <  < )
(define-numeric-binop lua-<= <= <= <=)
(define-numeric-binop lua->  >  >  > )
(define-numeric-binop lua->= >= >= >=)


;; length ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.lua.org/manual/5.4/manual.html#3.4.7

(provide
 (rename-out
  [lua-length #%length]))

(define (lua-length v)
  (cond
    [(bytes? v)
     (bytes-length v)]
    [(table? v)
     (table-length v)]
    [else
     (raise-argument-error "#" "a string or a table" v)]))


;; tables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (rename-out
  [make-table #%table]
  [table-ref #%table-ref]))

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
         (when (and (integer? k)
                    (> k border))
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
  (hash-ref (table-ht t) v lua-nil))
