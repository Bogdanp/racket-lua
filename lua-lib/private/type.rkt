#lang racket/base

(require "nil.rkt"
         "table.rkt")

(provide
 lua:type)

(define (lua:type v . _)
  (cond
    [(nil? v) #"nil"]
    [(table? v) #"table"]
    [(bytes? v) #"string"]
    [(number? v) #"number"]
    [(boolean? v) #"boolean"]
    [(procedure?* v) #"procedure"]
    [else (raise-argument-error 'lua:type "unexpected value" v)]))
