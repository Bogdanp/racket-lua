#lang racket/base

(require racket/path
         racket/port
         racket/runtime-path
         rackunit)

(define-runtime-path examples
  "examples")

(define compiler-tests
  (test-suite
   "compiler"

   (test-suite
    "examples"

    (for ([path (in-list (directory-list examples #:build? #t))]
          #:when (path-has-extension? path #".lua")
          #:unless (equal? (string->path "kitchen-sink.lua") (file-name-from-path path)))
      (define stdout-path
        (path-replace-extension path #".stdout.rktd"))
      (define stdout
        (call-with-output-string
         (lambda (out)
           (parameterize ([current-output-port out])
             (dynamic-require `(file ,(path->string path)) #f)))))
      (with-handlers ([exn:fail:filesystem?
                       (λ (_)
                         (call-with-output-file stdout-path
                           #:exists 'replace
                           (lambda (out)
                             (write stdout out))))])
        (define expected-stdout
          (call-with-input-file stdout-path read))
        (check-equal? stdout expected-stdout (path->string path)))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests compiler-tests))
