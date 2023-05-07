#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         (prefix-in racket: racket/base)
         "base.rkt"
         (prefix-in lua: "private/table.rkt"))

(provide
 (except-out (all-from-out "base.rkt") #%top #%set!)
 (rename-out
  [repl:top #%top]
  [repl:set! #%set!]))

(#%define _ENV (#%global))
(#%load-stdlib! _ENV)

(begin-for-syntax
  (define (id-stx->bytes-stx stx)
    (datum->syntax stx (string->bytes/utf-8 (symbol->string (syntax->datum stx))))))

(define-syntax (repl:top stx)
  (syntax-parse stx
    [(_ . id:id)
     #:with name (id-stx->bytes-stx #'id)
     #'(#%subscript _ENV name)]))

(define-syntax (repl:set! stx)
  (syntax-parse stx
    #:literals (#%subscript)
    [(_ (#%subscript t:expr k:expr) v:expr)
     #'(lua:table-set! t k v)]
    [(_ id:id e:expr)
     #:when (identifier-binding #'id)
     #'(racket:set! id e)]
    [(_ id:id e:expr)
     #:with name (id-stx->bytes-stx #'id)
     #'(lua:table-set! _ENV name e)]))

(module configure-runtime racket/base
  (require (submod "lang/compiler.rkt" private)
           "lang/lexer.rkt"
           "lang/parser.rkt"
           "private/string.rkt"
           racket/port
           racket/pretty
           syntax/strip-context)

  ;; Avoid depending on readline-lib.
  (define readline-prompt
    (or (dynamic-require 'readline/pread 'readline-prompt)
        (make-parameter #f)))

  (define debug?
    (not (not (getenv "RACKET_LUA_DEBUG"))))

  (define (read-interaction src in)
    (define (interact code-in)
      (define code
        (strip-context
         #`(#%let/cc #%return
             #,(compile-block
                (parameterize ([current-source-name src])
                  (parse-chunk (make-lexer code-in))))
             (#%values))))
      (begin0 code
        (when debug?
          (pretty-print (syntax->datum code)))))
    (define line (read-line in))
    (cond
      [(eof-object? line) eof]
      [(string=? ":{" line)
       (interact
        (open-input-string
         (call-with-output-string
          (lambda (out)
            (let loop ()
              (parameterize ([readline-prompt #"| "])
                (define code-line (read-line in))
                (unless (string=? ":}" code-line)
                  (displayln code-line out)
                  (loop))))))))]
      [else
       (interact (open-input-string line))]))

  (define (this-print v)
    (unless (void? v)
      (lua:print v)))

  (current-read-interaction read-interaction)
  (current-print this-print))
