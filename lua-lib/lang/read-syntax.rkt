#lang racket/base

(require syntax/readerr
         syntax/strip-context
         "compiler.rkt"
         "lexer.rkt"
         "parser.rkt")

(provide
 read-lua-syntax)

(define (read-lua-syntax src in)
  (define stmts
    (strip-context
     (compile-chunk
      (parameterize ([current-source-name src])
        (with-handlers ([exn:fail:lexer? reraise-lexer-error])
          (parse-chunk (make-lexer in)))))))
  (begin0 stmts
    (debug src stmts)))

(define (debug src stmts)
  (when (getenv "RACKET_LUA_DEBUG")
    (printf "<<~a>>~n" src)
    ((dynamic-require 'racket/pretty 'pretty-print)
     (syntax->datum stmts))))

(define (reraise-lexer-error e)
  (raise-read-error
   (exn-message e)
   (current-source-name)
   (exn:fail:lexer-line e)
   (exn:fail:lexer-col e)
   (exn:fail:lexer-pos e)
   #f))
