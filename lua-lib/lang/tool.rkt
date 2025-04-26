#lang racket/base

(require racket/class
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
       [(keyword) 'keyword]
       [(lparen rparen lsqbrace rsqbrace lcubrace rcubrace comma commacomma dot dotdot dotdotdot) 'parenthesis]
       [(name) 'symbol]
       [(number) 'constant]
       [(op) 'operator]
       [(whitespace) 'white-space]
       [else (token-type t)])
     (case (token-type t)
       [(lparen rparen lsqbrace rsqbrace lcubrace rcubrace) (string->symbol s)]
       [else #f])
     (and (not (eof-object? s))    (token-pos t))
     (and (not (eof-object? s)) (+ (token-pos t) (string-length s))))))

;; NOTE: These reqexp-quotes should really be pregexp-quotes, but
;; pregexp-quote was only added in ~8.12. The quoted expressions in
;; this case don't have any special pregexp characters, so this is
;; fine. Just something to keep in mind if this needs to expand.
(define indent-phrase-re
  (let ([phrases '("function" "local function" "if" "elseif" "else" "for" "while" "do" "repeat")])
    (pregexp (string-append "[[:space:]]*" "(" (string-join (map regexp-quote phrases) "|") ")"))))

(define indent-sym-re
  (regexp (string-append "[" (regexp-quote (string #\( #\[ #\{)) "]$")))

(define dedent-re
  #px"^[[:space:]]+(elseif|else|end|until|[)}\\]])[[:space:]]*")

(define tab-size 4)

(define (get-indent-size line)
  (string-length (car (regexp-match #px"[[:space:]]*" line))))

(define (get-indentation t pos)
  (define prev-line
    (let-values ([(s _e) (get-line-range t pos)])
      (and (positive? s)
           (get-line t (sub1 s)))))
  (define this-line
    (get-line t pos))
  (cond
    [(not prev-line) #f]
    [(regexp-match? dedent-re this-line)
     (max 0 (- (get-indent-size prev-line) tab-size))]
    [else
     (define prev-indent (get-indent-size prev-line))
     (if (or (regexp-match? indent-phrase-re prev-line)
             (regexp-match? indent-sym-re prev-line))
         (+ prev-indent tab-size)
         prev-indent)]))

(define (get-line-range t pos)
  (define para
    (send t position-paragraph pos #t))
  (values
   (send t paragraph-start-position para)
   (send t paragraph-end-position para)))

(define (get-line t pos)
  (define-values (s e)
    (get-line-range t pos))
  (send t get-text s e))
