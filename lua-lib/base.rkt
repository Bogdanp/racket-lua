#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         "private/env.rkt"
         "private/error.rkt"
         "private/nil.rkt"
         "private/string.rkt"
         "private/table.rkt")

;; kernel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 #%adjust
 #%adjust-va
 #%app
 #%datum
 #%subscript
 #%va-args
 (rename-out
  [#%plain-module-begin #%module-begin]
  [apply #%apply]
  [begin #%begin]
  [call/cc #%call/cc]
  [cond #%cond]
  [cons #%cons]
  [define #%define]
  [dynamic-wind #%dynamic-wind]
  [else #%else]
  [lambda #%lambda]
  [let #%let]
  [let/ec #%let/ec]
  [nil? #%nil?]
  [provide #%provide]
  [lua:error #%error]
  [lua:set! #%set!]
  [lua:top #%top]
  [lua:unless #%unless]
  [lua:when #%when]
  [time #%time]
  [truthy? #%truthy?]
  [values #%values]
  [void #%void]))

(begin-for-syntax
  (define (id-stx->bytes-stx stx)
    (datum->syntax stx (string->bytes/utf-8 (symbol->string (syntax->datum stx))))))

(define-syntax (lua:top stx)
  (syntax-parse stx
    [(_ . id:id)
     #:with name (id-stx->bytes-stx #'id)
     #:with env (format-id #'id "_ENV")
     #'(#%subscript env name)]))

(define-syntax (lua:set! stx)
  (syntax-parse stx
    #:literals (#%subscript)
    [(_ (#%subscript t:expr k:expr) v:expr)
     #'(table-set! t k v)]
    [(_ id:id e:expr)
     #:when (identifier-binding #'id)
     #'(set! id e)]
    [(_ id:id e:expr)
     #:with name (id-stx->bytes-stx #'id)
     #:with env (format-id #'id "_ENV")
     #'(table-set! env name e)]))

(define-syntax (lua:unless stx)
  (syntax-parse stx
    [(_ cond-expr body ...+)
     #'(unless (truthy? cond-expr)
         body ...)]))

(define-syntax (lua:when stx)
  (syntax-parse stx
    [(_ cond-expr body ...+)
     #'(when (truthy? cond-expr)
         body ...)]))

(define-syntax (#%subscript stx)
  (syntax-parse stx
    [(_ t:expr k:expr)
     #'(table-ref t k)]))

(define-syntax (#%adjust stx)
  (syntax-parse stx
    [(_ e)
     #'(call-with-values
        (lambda () e)
        (lambda vals (if (null? vals) nil (car vals))))]))

(define-syntax (#%adjust-va stx)
  (syntax-parse stx
    [(_ e)
     #'(call-with-values (lambda () e) list)]))

(define-syntax (#%va-args stx)
  (syntax-parse stx
    [(#%va-args)
     #:with #%rest (format-id #'#%va-args "#%rest")
     #'(apply values #%rest)]))


;; global environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.lua.org/manual/5.4/manual.html#2.2

(provide
 (rename-out
  [current-global-environment #%global]))


;; arithmetic operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.lua.org/manual/5.4/manual.html#3.4.1

(provide
 (rename-out
  [lua:unary-- #%unary-minus]
  [lua:+ +]
  [lua:- -]
  [lua:* *]
  [lua:/ /]
  [lua:// //]
  [lua:% %]
  [lua:pow ^]))

(define-syntax-rule (define-numeric-unop id who integer-proc real-proc)
  (define (id x)
    (cond
      [(integer? x) (integer-proc x)]
      [(real? x) (real-proc x)]
      [else (raise-argument-error 'who "expected a number" x)])))

(define-numeric-unop lua:unary-- '- - -)

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

(define (lua:real-modulo x y)
  (modulo
   (floor x)
   (floor y)))

(define (lua:real-quotient x y)
  (floor (/ x y)))

(define-numeric-binop lua:+ + + +)
(define-numeric-binop lua:- - - -)
(define-numeric-binop lua:* * * *)
(define-numeric-binop lua:% % modulo lua:real-modulo)
(define-numeric-binop lua:// // quotient lua:real-quotient)

(define (lua:/ x y)
  (cond
    [(and (number? x)
          (number? y))
     (/ (exact->inexact x)
        (exact->inexact y))]

    [else
     (raise-arguments-error '/ "expected two numbers" "x" x "y" y)]))

(define lua:pow
  (procedure-rename expt 'pow))


;; relational operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.lua.org/manual/5.4/manual.html#3.4.4

(provide
 (rename-out
  [lua:and and]
  [lua:or or]
  [lua:not not]
  [lua:== ==]
  [lua:~= ~=]
  [lua:<  < ]
  [lua:<= <=]
  [lua:>  > ]
  [lua:>= >=]))

(define (lua:== a b)
  (or (equal? a b)
      (and (table? a)
           (table? b)
           (or (let ([eq (table-meta-ref a #"__eq")])
                 (and (procedure? eq) (eq a b) #t))
               (let ([eq (table-meta-ref b #"__eq")])
                 (and (procedure? eq) (eq b a) #t))))))

(define (lua:~= a b)
  (not (lua:== a b)))

(define-numeric-binop lua:<  <  <  < )
(define-numeric-binop lua:<= <= <= <=)
(define-numeric-binop lua:>  >  >  > )
(define-numeric-binop lua:>= >= >= >=)

(define (lua:not v)
  (if (nil? v) #t (not v)))

(define-syntax-rule (lua:and a b)
  (if (falsy? a) nil b))

(define-syntax-rule (lua:or a b)
  (if (falsy? a) b a))


;; length ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.lua.org/manual/5.4/manual.html#3.4.7

(provide
 (rename-out
  [lua:length #%length]))

(define (lua:length v)
  (cond
    [(bytes? v)
     (bytes-length v)]
    [(table? v)
     (table-length v)]
    [else
     (raise-argument-error "#" "a string or a table" v)]))


;; basics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 nil
 (rename-out
  [concat ..]))

(define (concat a b)
  (bytes-append
   (lua:tostring a)
   (lua:tostring b)))


;; tables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (rename-out
  [make-table #%table]
  [table-ref #%table-ref]))
