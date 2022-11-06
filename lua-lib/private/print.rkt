#lang racket/base

(require racket/format
         "nil.rkt"
         "table.rkt")

(provide
 lua:string
 lua:print)

(define (lua:string v)
  (string->bytes/utf-8
   (cond
     [(eq? v nil) "nil"]
     [(table? v) (format "table: 0x~a" (number->string (eq-hash-code v) 16))]
     [else (~a v)])))

(define (lua:print . vs)
  (for ([v (in-list vs)])
    (display (lua:string v)))
  (newline))
