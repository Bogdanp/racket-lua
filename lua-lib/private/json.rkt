#lang racket/base

(require json
         "error.rkt"
         "nil.rkt"
         "string.rkt"
         "table.rkt")

(provide
 make-json-module)

(define (make-json-module)
  (make-table
   `(#"decode" . ,decode)
   `(#"encode" . ,encode)))

(define (decode s . _)
  (jsexpr->lua-value (bytes->jsexpr s)))

(define (encode v . _)
  (jsexpr->bytes (lua-value->jsexpr v)))

(define (lua-value->jsexpr v)
  (define js-null (json-null))
  (let loop ([v v])
    (cond
      [(nil? v) js-null]
      [(boolean? v) v]
      [(number? v) v]
      [(bytes? v)
       (bytes->string/utf-8 v)]
      [(array? v)
       (map loop (table-values v))]
      [(table? v)
       (for/hasheq ([(k v) (in-hash (table-ht v))])
         (unless (bytes? k)
           (lua:error (format "json.encode: object keys must be strings, received ~a" (lua:tostring k))))
         (define k-sym (string->symbol (bytes->string/utf-8 k)))
         (values k-sym (loop v)))]
      [else
       (lua:error (format "json.encode: unexpected value ~a" (lua:tostring v)))])))

(define (jsexpr->lua-value v)
  (define js-null (json-null))
  (let loop ([v v])
    (cond
      [(eq? v js-null) nil]
      [(boolean? v) v]
      [(number? v) v]
      [(bytes? v)
       (string->bytes/utf-8 v)]
      [(list? v)
       (apply make-table (map loop v))]
      [(hash? v)
       (apply make-table (for/list ([(k v) (in-hash v)])
                           (define k-str (string->bytes/utf-8 (symbol->string k)))
                           (cons k-str (loop v))))]
      [else
       (raise-argument-error 'jsexpr->lua-value "jsexpr?" v)])))

;; TODO: Sparse check.
(define (array? v)
  (and (table? v)
       (not (nil? (lua:rawget v 1)))))
