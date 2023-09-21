#lang racket/base

(require racket/class
         racket/port
         racket/string
         "lexer.rkt")

(provide
 get-color-token
 get-indentation)

(define (get-color-token in)
  (with-handlers ([exn:fail:lexer?
                   (lambda (e)
                     (values "" 'error #f (exn:fail:lexer-pos e) (add1 (exn:fail:lexer-pos e))))])
    (define l (make-lexer in #:skip-comments? #f #:partial-strings? #t))
    (define t (lexer-take l))
    (define s (token-str t))
    (values
     s
     (case (token-type t)
       [(whitespace) 'white-space]
       [(lparen rparen lsqbrace rsqbrace lcubrace rcubrace comma commacomma dot dotdot dotdotdot) 'parenthesis]
       [(op keyword) 'keyword]
       [(number) 'constant]
       [(name) 'symbol]
       [else (token-type t)])
     (case (token-type t)
       [(lparen rparen lsqbrace rsqbrace lcubrace rcubrace) (string->symbol s)]
       [else #f])
     (and (not (eof-object? s))    (token-pos t))
     (and (not (eof-object? s)) (+ (token-pos t) (string-length s))))))

(define indent-phrase-re
  (let ([phrases '("function" "local function" "if" "elseif" "else" "for" "while" "do" "repeat")])
    (pregexp (string-append "[[:space:]]*" "(" (string-join (map regexp-quote phrases) "|") ")"))))

(define indent-sym-re
  (regexp (string-append "[" (regexp-quote (string #\( #\[ #\{)) "]$")))

(define dedent-re
  #px"[[:space:]]{4,}(elseif|else|end|until|[)}\\]])[[:space:]]*")

(define tab-size 4)

(define (get-indent-size line)
  (string-length (car (regexp-match #px"[[:space:]]*" line))))

;; FIXME: This isn't very efficient, but I can't figure out a non-hacky
;; way to get the previous line's indentation by limiting myself to the
;; color-textoid<%> interface so it'll have to do for now.
;;
;; xref: https://github.com/greghendershott/racket-mode/issues/668#issuecomment-1728081515
(define (get-indentation editor pos)
  (define-values (prev-line this-line)
    (call-with-input-string
      (string-append (send editor get-text 0 'eof) "\n")
      (lambda (in)
        (for/fold ([cursor 0]
                   [prev-line #f]
                   [this-line #f]
                   #:result (values prev-line this-line))
                  ([line (in-lines in 'linefeed)])
          #:break (> cursor pos)
          (values (+ cursor (string-length line) 1) this-line line)))))
  (cond
    [(not prev-line) #f]
    [(regexp-match-exact? dedent-re this-line)
     (- (get-indent-size this-line) tab-size)]
    [else
     (define prev-indent (get-indent-size prev-line))
     (if (or (regexp-match? indent-phrase-re prev-line)
             (regexp-match? indent-sym-re prev-line))
         (+ prev-indent tab-size)
         prev-indent)]))
