#lang racket/base

(require racket/format
         "nil.rkt"
         "table.rkt")

(provide
 current-print-table-ids?
 lua:tostring
 lua:print)

(define current-print-table-ids?
  (make-parameter #t))

(define (lua:tostring v)
  (string->bytes/utf-8
   (cond
     [(eq? v nil) "nil"]
     [(table? v)
      (if (current-print-table-ids?)
          (format "<table: 0x~a>" (number->string (eq-hash-code v) 16))
          "<table>")]
     [else (~a v)])))

(define (lua:print . vs)
  (for ([v (in-list vs)])
    (display (lua:tostring v)))
  (newline))
