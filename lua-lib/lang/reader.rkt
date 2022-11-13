(module reader syntax/module-reader
  #:language 'lua/base
  #:read custom-read
  #:read-syntax custom-read-syntax
  #:whole-body-readers? #t
  #:info get-info

  (require "read-syntax.rkt")

  (define (custom-read _in)
    (error 'read "not supported"))

  (define (custom-read-syntax src in)
    (read-lua-syntax src in))

  (define (get-info key defval default)
    (case key
      [(color-lexer) (dynamic-require 'lua/lang/tool 'get-color-token)]
      [(drracket:default-filters) '(["Lua Script" "*.lua"])]
      [(drracket:default-extension) "lua"]
      [else (default key defval)])))
