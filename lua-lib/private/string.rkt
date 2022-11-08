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

(define (lua:tostring v . _)
  (define str
    (cond
      [(eq? v #t) "true"]
      [(eq? v #f) "false"]
      [(eq? v nil) "nil"]
      [(table? v)
       (define custom-tostring
         (table-meta-ref v #"__tostring"))
       (cond
         [(nil? custom-tostring)
          (define name (table-meta-ref v #"__name" (λ () #"table")))
          (->string name v)]
         [else
          (custom-tostring v)])]
      [(procedure? v)
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
