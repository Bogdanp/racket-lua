#lang racket/base

(require racket/contract/base
         "private/env.rkt"
         "private/table.rkt")

(provide
 (contract-out
  [make-initial-environment (-> table?)]
  [current-racket-imports-enabled? (parameter/c boolean?)]
  [current-global-environment (parameter/c table?)]
  [current-standard-library-modules (parameter/c
                                     (listof
                                      (cons/c bytes? (or/c module-path?
                                                           resolved-module-path?
                                                           module-path-index?
                                                           (-> table? bytes? void?)))))]))
