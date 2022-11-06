(module reader syntax/module-reader
  #:language 'lua/base
  #:read custom-read
  #:read-syntax custom-read-syntax
  #:whole-body-readers? #t

  (require syntax/strip-context
           "compiler.rkt"
           "lexer.rkt"
           "parser.rkt")

  (define (custom-read _in)
    (error 'custom-read "not implemented"))

  (define (custom-read-syntax src in)
    (define stmts
      (strip-context
       (compile-chunk
        (parameterize ([current-source-name src])
          (parse-chunk (make-lexer in))))))
    (begin0 stmts
      (debug stmts)))

  (define debug
    (if (getenv "RACKET_LUA_DEBUG")
        (compose1 (dynamic-require 'racket/pretty 'pretty-print) syntax->datum)
        void)))
