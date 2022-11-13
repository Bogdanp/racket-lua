#lang racket/base

(require racket/port)

(define (make-#lang-lua-port src-in)
  (define-values (in out)
    (make-pipe))
  (begin0 in
    (thread
     (lambda ()
       (displayln "#lang lua" out)
       (copy-port src-in out)
       (close-output-port out)))))

(module+ main
  (require racket/cmdline
           racket/sandbox)

  (define ins
    (map make-#lang-lua-port
         (command-line
          #:args paths
          (if (null? paths)
              (list (current-input-port))
              (map open-input-file paths)))))

  (call-with-trusted-sandbox-configuration
   (lambda ()
     (parameterize ([sandbox-input (current-input-port)]
                    [sandbox-output (current-output-port)]
                    [sandbox-error-output (current-error-port)])
       (for ([in (in-list ins)])
         ((make-module-evaluator in) '#%chunk))))))
