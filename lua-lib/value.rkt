#lang racket/base

(require racket/contract
         "private/nil.rkt"
         "private/table.rkt")

(provide
 lua-value/c

 nil?
 nil

 table?
 (contract-out
  [make-table (-> (or/c lua-value/c (cons/c lua-value/c lua-value/c)) ... table?)]
  [table-ref (->* (table? lua-value/c) ((-> lua-value/c)) lua-value/c)]
  [table-set! (-> table? lua-value/c lua-value/c void?)]
  [table-length (-> table? exact-nonnegative-integer?)]
  [lua:getmetatable (-> table? (or/c nil? table?))]
  [lua:setmetatable (-> table? table? table?)]))

(define lua-value/c
  (or/c boolean? bytes? number? nil? procedure? table?))
