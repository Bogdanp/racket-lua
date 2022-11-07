#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide
 nil?
 nil
 nil~>)

(define (nil? v)
  (eq? nil v))

(define nil
  (gensym 'nil))

(define-syntax (nil~> stx)
  (syntax-parse stx
    [(_ a) #'a]
    [(_ a (rator rand ...)) #'(if (nil? a) nil (rator a rand ...))]
    [(_ a b c ...) #'(nil~> (nil~> a b) c ...)]))
