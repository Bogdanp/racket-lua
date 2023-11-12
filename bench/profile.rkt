#lang racket/base

(require profile
         racket/cmdline
         racket/file
         racket/port)

(define (eval-lua lua-in args)
  (define path (make-temporary-file "lua~a.lua"))
  (call-with-output-file path
    #:exists 'truncate/replace
    (lambda (out)
      (displayln "#lang lua" out)
      (copy-port lua-in out)))
  (parameterize ([current-command-line-arguments (list->vector args)])
    (dynamic-require path '#%chunk)))

(module+ main
  (require racket/cmdline)

  (define profile? #t)
  (define-values (program args)
    (command-line
     #:once-each
     [("-q") "disable profiling" (set! profile? #f)]
     #:args [program . args]
     (values program args)))

  (call-with-input-file program
    (lambda (in)
      (if profile?
          (profile-thunk
           #:delay 0
           #:use-errortrace? #t
           (lambda () (eval-lua in args)))
          (time (eval-lua in args))))))
