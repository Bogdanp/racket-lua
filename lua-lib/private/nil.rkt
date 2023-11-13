#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/lazy-require
         racket/port
         "exn.rkt")

(lazy-require
 ["string.rkt" (lua:tostring)])

(provide
 nil?
 nil
 nil~>
 falsy?
 truthy?
 procedure?*)

(define nil
  (let ()
    (struct nil ()
      #:methods gen:custom-write
      [(define (write-proc _ out _mode)
         (display "nil" out))]
      #:property prop:procedure
      (lambda (_ . args)
        (cond
          [(null? args)
           (raise-lua-error #f "attempt to call a nil value")]
          [else
           (define args-str
             (call-with-output-string
              (lambda (out)
                (for ([arg (in-list args)])
                  (define arg-str
                    (bytes->string/utf-8 (lua:tostring arg) #\uFFFD))
                  (fprintf out "~n   ~a" arg-str)))))
           (raise-lua-error #f (format "attempt to call a nil value~n  call args: ~a" args-str))])))
    (nil)))
(define (nil? v)
  (eq? v nil))

(define-syntax (nil~> stx)
  (syntax-parse stx
    [(_ a) #'a]
    [(_ a (rator rand ...)) #'(if (nil? a) nil (rator a rand ...))]
    [(_ a b c ...) #'(nil~> (nil~> a b) c ...)]))

(define (falsy? v)
  (or (nil? v) (not v)))

(define (truthy? v)
  (and (not (nil? v)) v))

(define (procedure?* v)
  (and (procedure? v)
       (not (nil? v))))
