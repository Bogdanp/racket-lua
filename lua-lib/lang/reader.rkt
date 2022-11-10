(module reader syntax/module-reader
  #:language 'lua/base
  #:read custom-read
  #:read-syntax custom-read-syntax
  #:whole-body-readers? #t
  #:info get-info

  (require syntax/readerr
           syntax/strip-context
           "compiler.rkt"
           "lexer.rkt"
           "parser.rkt")

  (define (debug src stmts)
    (when (getenv "RACKET_LUA_DEBUG")
      (printf "<<~a>>~n" src)
      ((dynamic-require 'racket/pretty 'pretty-print)
       (syntax->datum stmts))))

  (define (custom-read _in)
    (error 'read "not supported"))

  (define (custom-read-syntax src in)
    (define stmts
      (strip-context
       (compile-chunk
        (parameterize ([current-source-name src])
          (with-handlers ([exn:fail:lexer? reraise-lexer-error])
            (parse-chunk (make-lexer in)))))))
    (begin0 stmts
      (debug src stmts)))

  (define (get-info key defval default)
    (case key
      [(color-lexer) (dynamic-require 'lua/lang/tool 'get-color-token)]
      [(drracket:default-filters) '(["Lua Script" "*.lua"])]
      [(drracket:default-extension) "lua"]
      [else (default key defval)]))

  (define (reraise-lexer-error e)
    (raise-read-error
     (exn-message e)
     (current-source-name)
     (exn:fail:lexer-line e)
     (exn:fail:lexer-col e)
     (exn:fail:lexer-pos e)
     #f)))
