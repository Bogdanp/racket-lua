#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide
 nil?
 nil
 nil~>
 falsy?
 truthy?)

(define nil  null)
(define nil? null?)

(define-syntax (nil~> stx)
  (syntax-parse stx
    [(_ a) #'a]
    [(_ a (rator rand ...)) #'(if (nil? a) nil (rator a rand ...))]
    [(_ a b c ...) #'(nil~> (nil~> a b) c ...)]))

(define (falsy? v)
  (or (nil? v) (not v)))

(define (truthy? v)
  (and (not (nil? v)) v))
