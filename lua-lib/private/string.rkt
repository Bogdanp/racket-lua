#lang racket/base

(require racket/format
         "nil.rkt"
         "table.rkt")

(provide
 current-print-ids?
 lua:tostring
 lua:print)

(define current-print-ids?
  (make-parameter #t))

(define (->string what v)
  (if (current-print-ids?)
      (format "<~a: 0x~a>" what (number->string (eq-hash-code v) 16))
      (format "<~a>" what)))

(define (lua:tostring v)
  (string->bytes/utf-8
   (cond
     [(eq? v nil) "nil"]
     [(procedure? v) (->string "function" v)]
     [(table? v) (->string "table" v)]
     [else (~a v)])))

(define (lua:print . vs)
  (for ([i (in-naturals)]
        [v (in-list vs)])
    (unless (zero? i)
      (display #\tab))
    (display (lua:tostring v)))
  (newline))
