#lang racket/base

(require racket/contract
         "private/env.rkt"
         "private/table.rkt")

(provide
 (contract-out
  [current-global-environment (parameter/c table?)]
  [make-initial-environment (-> table?)]))
