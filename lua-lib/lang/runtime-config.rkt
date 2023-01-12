#lang racket/base

(require racket/match
         "../private/realm.rkt")

(current-error-message-adjuster
 (lambda (protocol)
   (case protocol
     [(message)
      (lambda (name name-realm message message-realm)
        (match* (name name-realm)
          [('application 'racket/primitive)
           #:when (regexp-match? #rx"not a procedure.*given: 'nil" message)
           (values name name-realm "attempt to call a nil value" lua-realm)]
          [(_ _)
           (values name name-realm message message-realm)]))]
     [else
      #f])))
