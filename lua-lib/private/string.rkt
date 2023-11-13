#lang racket/base

(require iso-printf
         iso-printf/custom
         racket/format
         racket/port
         "exn.rkt"
         "nil.rkt"
         "table.rkt")

(provide
 current-print-ids?
 current-string-metatable
 lua:tostring
 lua:print
 lua:format)

(define current-print-ids?
  (make-parameter #t))

(define current-string-metatable
  (make-parameter nil))

(define (->string what v)
  (if (current-print-ids?)
      (format "<~a: 0x~a>" what (number->string (eq-hash-code v) 16))
      (format "<~a>" what)))

(define (lua:toliteral v)
  (cond
    [(eq? v #t)  "true"]
    [(eq? v #f)  "false"]
    [(eq? v nil) "nil"]
    [(exact-integer? v)
     (number->string v 10)]
    [(number? v)
     (~a "0x"
         (~r v
             #:base 16
             #:notation 'exponential
             #:format-exponent "p"))]
    [(bytes? v)
     (quote-string (bytes->string/utf-8 v))]
    [else
     (raise-lua-error 'string.format "value has no literal form")]))

(define (lua:tostring v . _)
  (define str
    (cond
      [(eq? v #t) "true"]
      [(eq? v #f) "false"]
      [(eq? v nil) "nil"]
      [(table? v)
       (define dunder-proc
         (table-meta-ref v #"__tostring"))
       (cond
         [(nil? dunder-proc)
          (define name (table-meta-ref v #"__name" (λ () #"table")))
          (->string name v)]
         [else
          (dunder-proc v)])]
      [(procedure?* v)
       (->string "function" v)]
      [else
       (~a v)]))
  (cond
    [(bytes? str) str]
    [else (string->bytes/utf-8 str)]))

(define (lua:print . vs)
  (for ([i (in-naturals)]
        [v (in-list vs)])
    (unless (zero? i)
      (display #\tab))
    (display (lua:tostring v)))
  (newline))

(define custom-printf-table
  (hasheq #\q (lambda (flags arg width precision)
                (define (formatter _precision v)
                  (values "" (lua:toliteral v)))
                (directive arg flags width precision formatter nil #t))))

(define (lua:format fmt . args)
  (parameterize ([current-custom-conversions custom-printf-table])
    (string->bytes/utf-8 (apply sprintf (bytes->string/utf-8 fmt) args))))

(define (quote-string s)
  (define escaped
    (call-with-output-string
     (lambda (out)
       (for/fold ([escaped? #f])
                 ([c (in-string s)])
         (case c
           [(#\\ #\" #\newline)
            (cond
              [escaped?
               (begin0 #f
                 (write-char c out))]
              [(eqv? #\\ c)
               (begin0 #t
                 (write-char c out))]
              [else
               (begin0 #f
                 (write-char #\\ out)
                 (write-char c out))])]
           [else
            (begin0 #f
              (write-char c out))])))))
  (string-append "\"" escaped "\""))
