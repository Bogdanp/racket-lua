#lang racket/base

(require lua/lang/lexer
         racket/path
         racket/pretty
         racket/runtime-path
         rackunit)

(define-runtime-path examples
  "examples")

(define (lexer->tokens l)
  (let loop ([tokens null])
    (define token (lexer-take l))
    (case (token-type token)
      [(eof) (reverse tokens)]
      [else (loop (cons token tokens))])))

(define lexer-tests
  (test-suite
   "lexer"

   (test-suite
    "lexer-peek"

    (test-case "gives at most one token of lookahead"
      (define l (make-lexer (open-input-string "fib(1 + 2)")))
      (check-equal? (lexer-peek l) (token 'name "fib" 'fib 1 0 1))
      (check-equal? (lexer-peek l) (token 'name "fib" 'fib 1 0 1))
      (check-equal? (lexer-take l) (token 'name "fib" 'fib 1 0 1))
      (check-equal? (lexer-peek l) (token 'lparen "(" "(" 1 3 4))
      (check-equal? (lexer-peek l) (token 'lparen "(" "(" 1 3 4))))

   (test-suite
    "examples"

    (for ([path (in-list (directory-list examples #:build? #t))]
          #:when (path-has-extension? path #".lua"))
      (define tokens-path
        (path-replace-extension path #".tokens.rktd"))
      (define tokens
        (call-with-input-file path
          (lambda (in)
            (for/hasheqv ([idx (in-naturals)]
                          [tok (in-list (lexer->tokens (make-lexer in #:skip-comments? #f)))])
              (values idx tok)))))
      (with-handlers ([exn:fail:filesystem?
                       (λ (_)
                         (call-with-output-file tokens-path
                           #:exists 'replace
                           (lambda (out)
                             (pretty-write tokens out))))])
        (define expected-tokens
          (call-with-input-file tokens-path read))
        (define n
          (max (hash-count tokens)
               (hash-count expected-tokens)))
        (for ([idx (in-range n)])
          (define a (hash-ref tokens idx #f))
          (define b (hash-ref expected-tokens idx #f))
          (check-equal? a b (format "~a#~a" path idx))))))))

(module+ test
  (require rackunit/text-ui)
  (parameterize ([port-count-lines-enabled #t])
    (run-tests lexer-tests)))
