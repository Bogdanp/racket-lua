#lang racket/base

(require racket/match
         racket/port)

(provide
 (struct-out exn:fail:lexer)
 (struct-out token)

 make-lexer
 lexer-peek
 lexer-take)

(struct exn:fail:lexer exn:fail (line col pos)
  #:transparent)

(define (raise-lexer-error message line col pos)
  (raise (exn:fail:lexer message (current-continuation-marks) line col pos)))

(struct token (type str val line col pos)
  #:prefab)

(struct lexer (in skip? [pending #:mutable])
  #:transparent)

(define (make-lexer in [skip? #t])
  (lexer in skip? #f))

(define (lexer-peek l)
  (cond
    [(lexer-pending l) => values]
    [else
     (define pending (lexer-read-token l))
     (begin0 pending
       (set-lexer-pending! l pending))]))

(define (lexer-take l)
  (cond
    [(lexer-pending l)
     => (lambda (pending)
          (begin0 pending
            (set-lexer-pending! l #f)))]

    [else
     (lexer-read-token l)]))

(define (lexer-read-token l)
  (define skip? (lexer-skip? l))
  (let loop ()
    (define t (read-token (lexer-in l)))
    (case (token-type t)
      [(comment whitespace)
       (if skip? (loop) t)]
      [else t])))


;; readers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-token in)
  (define-values (line col pos)
    (port-next-location in))

  (define (make-token type str [val str])
    (token type str val line col pos))

  (match (peek-char in)
    [(? eof-object?) (make-token 'eof        (read-string 1 in))]
    [(? whitespace?) (make-token 'whitespace (read-whitespace in))]

    ;; TODO: Long brackets.
    [#\- #:when (eqv? #\- (peek-char in 1))
     (make-token 'comment (read-line in))]

    [#\: #:when (equal? "::" (peek-string 2 0 in))
     (make-token 'coloncolon (read-string 2 in))]
    [#\:
     (make-token 'colon (read-string 1 in))]

    [#\; (make-token 'semicolon  (read-string 1 in))]
    [#\( (make-token 'lparen     (read-string 1 in))]
    [#\) (make-token 'rparen     (read-string 1 in))]
    [#\[ (make-token 'lsqbrace   (read-string 1 in))]
    [#\] (make-token 'rsqbrace   (read-string 1 in))]
    [#\{ (make-token 'lcubrace   (read-string 1 in))]
    [#\} (make-token 'rcubrace   (read-string 1 in))]
    [#\, (make-token 'comma      (read-string 1 in))]

    [#\. #:when (equal? "..." (peek-string 3 0 in))
     (make-token 'dotdotdot (read-string 3 in) '...)]
    [#\. #:when (equal? ".." (peek-string 2 0 in))
     (make-token 'op (read-string 2 in) '..)]
    [#\. #:when (not (number-digit? (peek-char in 1)))
     (make-token 'dot (read-string 1 in) '\.)]

    [(or #\' #\")
     (define-values (s v)
       (lua:read-string in))
     (make-token 'string s v)]

    [(? number-start?)
     (define-values (s v)
       (lua:read-number in))
     (make-token 'number s v)]

    [(? op-start?)
     (define-values (s v)
       (lua:read-op in))

     (make-token 'op s v)]

    [(? name-start?)
     (define-values (s v)
       (lua:read-name in))

     (case v
       [(and or not)
        (make-token 'op s v)]
       [(break do else elseif end false for function goto if in local nil repeat return then true until while)
        (make-token 'keyword s v)]
       [else
        (make-token 'name s v)])]

    [c
     (raise-lexer-error (format "unexpected character: ~a" c) line col pos)]))


;; helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-whitespace in)
  (take-while in whitespace?))

(define (take-while in p)
  (define-values (line col pos)
    (port-next-location in))

  (with-output-to-string
    (lambda ()
      (let loop ([p p]
                 [c (peek-char in)]
                 [span 0])
        (define next-p
          (with-handlers ([exn:fail? (λ (e) (raise-lexer-error (exn-message e) line col pos))])
            (p c)))

        (when next-p
          (display (read-char in))
          (loop next-p (peek-char in) (add1 span)))))))


;; matchers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (λcase [(lit ...) e ...] ...)
  (λ (next-c)
    (case next-c
      [(lit ...) e ...] ...
      [else #f])))

(define-syntax-rule (define-λcase name [(lit ...) e ...] ...)
  (define (name c)
    (case c
      [(lit ...) e ...] ...
      [else #f])))

(define (stop _) #f)

(define-λcase whitespace?
  [(#\space #\tab #\newline #\return) whitespace?])

(define-λcase op-start?
  [(#\+ #\- #\* #\^ #\% #\& #\| #\#) stop]
  [(#\/) (λcase [(#\/) stop])]
  [(#\<) (λcase [(#\< #\=) stop])]
  [(#\>) (λcase [(#\> #\=) stop])]
  [(#\=) (λcase [(#\=) stop])]
  [(#\~) (λcase [(#\=) stop])])

(define ((make-name-predicate [char-categories '(ll lu nd)]) c)
  (and
   (char? c)
   (or (char=? c #\_)
       (member (char-general-category c) char-categories))
   name-more?))

(define name-start? (make-name-predicate '(ll lu)))
(define name-more?  (make-name-predicate '(ll lu nd)))

(define (signed-number-start? c)
  (case c
    [(#\- #\+) number-start?]
    [else (number-start? c)]))

(define-λcase number-start?
  [(#\.) (number-digit-or-period? #\.)]
  [(#\0) (λcase [(#\.) number-digit?]) ]
  [(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) number-digit-or-period?])

(define (number-digit-or-period? c)
  (case c
    [(#\.)
     (lambda (next-c)
       (or (number-digit? next-c)
           (error "expected a digit")))]

    [else
     (number-digit? c)]))

(define-λcase number-digit?
  [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) number-digit?])


;; readers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((make-reader p f) in)
  (define s (take-while in p))
  (values s (f s)))

(define lua:read-name
  (make-reader name-start? string->symbol))

(define lua:read-op
  (make-reader op-start? string->symbol))

(define lua:read-number
  (make-reader signed-number-start? string->number))

(define (lua:read-string in)
  (define quote-char (read-char in))
  (define str
    (with-output-to-string
      (lambda ()
        (write-char quote-char)
        (let loop ([escaped? #f])
          (define char
            (read-char in))
          (cond
            [escaped?
             (write-char (lua:string-escape char))
             (loop #f)]
            [(eqv? char #\\)
             (loop #t)]
            [else
             (write-char char)
             (unless (eqv? char quote-char)
               (loop #f))])))))
  (define bs
    (string->bytes/utf-8
     (substring str 1 (sub1 (string-length str)))))
  (values str bs))

(define (lua:string-escape chr)
  (case chr
    [(#\\) #\\]
    [(#\r) #\return]
    [(#\n) #\newline]
    [(#\t) #\tab]
    [else chr]))
