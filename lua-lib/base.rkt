#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         "private/adjust.rkt"
         "private/env.rkt"
         "private/error.rkt"
         "private/length.rkt"
         "private/logic.rkt"
         "private/nil.rkt"
         "private/relation.rkt"
         "private/string.rkt"
         "private/table.rkt")

;; kernel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 #%app
 #%datum
 (rename-out
  [#%plain-module-begin #%module-begin]
  [apply #%apply]
  [begin #%begin]
  [call/cc #%call/cc]
  [cons #%cons]
  [define #%define]
  [dynamic-wind #%dynamic-wind]
  [else #%else]
  [lambda #%lambda]
  [let #%let]
  [let/ec #%let/ec]
  [provide #%provide]
  [time #%time]
  [truthy? #%truthy?]
  [values #%values]
  [void #%void])

 nil
 (rename-out
  [current-global-environment #%global]
  [lua:== ==]
  [lua:adjust #%adjust]
  [lua:adjust-va #%adjust-va]
  [lua:and and]
  [lua:cond #%cond]
  [lua:error #%error]
  [lua:length #%length]
  [lua:not not]
  [lua:or or]
  [lua:set! #%set!]
  [lua:subscript #%subscript]
  [lua:top #%top]
  [lua:unless #%unless]
  [lua:va-args #%va-args]
  [lua:when #%when]
  [lua:~= ~=]
  [make-table #%table]
  [table-ref #%table-ref]))

(begin-for-syntax
  (define (id-stx->bytes-stx stx)
    (datum->syntax stx (string->bytes/utf-8 (symbol->string (syntax->datum stx))))))

(define-syntax (lua:top stx)
  (syntax-parse stx
    [(_ . id:id)
     #:with name (id-stx->bytes-stx #'id)
     #:with env (format-id #'id "_ENV")
     #'(lua:subscript env name)]))

(define-syntax (lua:set! stx)
  (syntax-parse stx
    #:literals (lua:subscript)
    [(_ (lua:subscript t:expr k:expr) v:expr)
     #'(table-set! t k v)]
    [(_ id:id e:expr)
     #:when (identifier-binding #'id)
     #'(set! id e)]
    [(_ id:id e:expr)
     #:with name (id-stx->bytes-stx #'id)
     #:with env (format-id #'id "_ENV")
     #'(table-set! env name e)]))

(define-syntax (lua:cond stx)
  (syntax-parse stx
    #:literals (else)
    [(_ [test-expr body-expr ...+] ...
        [else else-body-expr ...+])
     #'(cond
         [(truthy? test-expr) body-expr ...] ...
         [else else-body-expr ...])]))

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

(define-syntax (lua:subscript stx)
  (syntax-parse stx
    [(_ t:expr k:expr)
     #'(table-ref t k)]))

(define-syntax (lua:adjust stx)
  (syntax-parse stx
    [(_ e) #'(lua:adjust* (λ () e))]))

(define-syntax (lua:adjust-va stx)
  (syntax-parse stx
    [(_ e) #'(lua:adjust-va* (λ () e))]))

(define-syntax (lua:va-args stx)
  (syntax-parse stx
    [(_)
     #:with #%rest (format-id stx "#%rest")
     #'(apply values #%rest)]))


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
  [lua:<  < ]
  [lua:<= <=]
  [lua:>  > ]
  [lua:>= >=]))

(define-numeric-binop lua:<  <  <  < )
(define-numeric-binop lua:<= <= <= <=)
(define-numeric-binop lua:>  >  >  > )
(define-numeric-binop lua:>= >= >= >=)



;; basics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (rename-out
  [concat ..]))

(define (concat a b)
  (bytes-append
   (lua:tostring a)
   (lua:tostring b)))
