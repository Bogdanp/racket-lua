#lang racket/base

(require racket/match
         "../private/realm.rkt")

(current-error-message-adjuster
 (lambda (protocol)
   (case protocol
     [(message)
      (lambda (name name-realm message message-realm)
        (match* (name name-realm)
          [(_ _)
           (values name name-realm message message-realm)]))]
     [else
      #f])))
