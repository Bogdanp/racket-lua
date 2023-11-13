#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/lazy-require
         racket/port
         racket/string
         "mark.rkt")

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
        (define args-str
          (string-join
           (for/list ([arg (in-list args)])
             (bytes->string/utf-8 (lua:tostring arg) #\uFFFD))
           ", "))
        (define stack-str
          (~call-stack))
        (define indented-stack-str
          (and stack-str
               (call-with-output-string
                (lambda (out)
                  (for ([line (in-lines (open-input-string stack-str))])
                    (fprintf out "   ~a~n" line))))))
        (if stack-str
            (error (format "attempt to call a nil value~n  call stack:~a  call args: ~a" indented-stack-str args-str))
            (error (format "attempt to call a nil value~n  call args: ~a" args-str)))))
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
