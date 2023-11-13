#lang racket/base

(require racket/match
         "../private/exn.rkt"
         "../private/mark.rkt"
         "../private/realm.rkt")

(provide
 configure-runtime)

(define (configure-runtime)
  (error-display-handler
   (let ([base-handler (error-display-handler)])
     (lambda (message e)
       (if (exn:fail:lua? e)
           (display message (current-error-port))
           (base-handler message e)))))

  (current-error-message-adjuster
   (lambda (protocol)
     (case protocol
       [(message)
        (lambda (name name-realm message message-realm)
          (match* (name name-realm)
            [(_ (== lua-realm))
             (define stack-str
               (~call-stack))
             (define adjusted-message
               (if stack-str
                   (format "~a~n  call stack:~a" message stack-str)
                   message))
             (values name name-realm adjusted-message message-realm)]
            [(_ _)
             (values name name-realm message message-realm)]))]
       [else
        #f]))))
