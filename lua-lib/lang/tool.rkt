#lang racket/base

(require "lexer.rkt")

(provide
 get-color-token)

(define (get-color-token in)
  (with-handlers ([exn:fail:lexer?
                   (lambda (e)
                     (values "" 'error #f (exn:fail:lexer-pos e) (add1 (exn:fail:lexer-pos e))))])
    (define l (make-lexer in #f))
    (define t (lexer-take l))
    (define s (token-str t))
    (values
     s
     (case (token-type t)
       [(whitespace) 'white-space]
       [(lparen rparen lsqbrace rsqbrace lcubrace rcubrace comma commacomma dot dotdot dotdotdot) 'parenthesis]
       [(op keyword) 'hash-colon-keyword]
       [(number) 'constant]
       [(name) 'symbol]
       [else (token-type t)])
     (case (token-type t)
       [(lparen rparen lsqbrace rsqbrace lcubrace rcubrace) (string->symbol s)]
       [else #f])
     (and (not (eof-object? s))    (token-pos t))
     (and (not (eof-object? s)) (+ (token-pos t) (string-length s))))))
