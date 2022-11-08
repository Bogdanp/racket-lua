#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/match
         syntax/parse
         syntax/readerr
         "ast.rkt"
         "lexer.rkt")

(provide
 current-source-name
 parse-chunk)

(define current-source-name
  (make-parameter #f))

(define-syntax-rule (define-token-matchers [name arg ...] ...)
  (begin
    (define-match-expander name
      (lambda (stx)
        (syntax-parse stx
          [(_ arg ...)
           #'(token 'name _ (and arg ... _) _ _ _)])))
    ...))

(define-token-matchers
  [eof]
  [coloncolon]
  [colon]
  [semicolon]
  [lparen]
  [rparen]
  [lsqbrace]
  [lcubrace]
  [rcubrace]
  [comma]
  [dotdotdot]
  [dot]
  [op e]
  [keyword e]
  [string e]
  [number e]
  [name e])

(define-match-expander binop
  (lambda (stx)
    (syntax-parse stx
      [(_ p:id)
       #'(op (app precedence p))])))

(define (right-associative? id)
  (and (memv id '(.. ^)) #t))

(define (precedence id)
  (case id
    [(or)              0]
    [(and)             1]
    [(< > <= >= ~= ==) 2]
    [(\|)              3]
    [(~)               3]
    [(&)               4]
    [(<< >>)           5]
    [(..)              6]
    [(+ -)             7]
    [(* / // %)        8]
    [(not)             9] ; unary
    [(#%length)        9] ; unary
    [(#%unary-minus)   9] ; unary
    [(#%bitwise-not)   9] ; unary
    [(^)               10]
    [else
     (raise-argument-error 'precedence "an operator" id)]))

(define (parse-chunk l)
  (begin0 (parse-block l)
    (skip l 'eof)))

(define (parse-block l [enders '(end)])
  (define ctxt
    (token-ctxt (lexer-peek l)))
  (let loop ([stmts null])
    (match (lexer-peek l)
      [(eof)
       (Block ctxt (reverse stmts))]
      [(keyword id)
       #:when (member id enders)
       (Block ctxt (reverse stmts))]
      [(keyword 'return)
       (define return-ctxt (token-ctxt (expect l 'keyword 'return)))
       (define return (Return return-ctxt (parse-exprs l)))
       (maybe-skip l 'semicolon)
       (Block ctxt (reverse (cons return stmts)))]
      [(semicolon)
       (skip l 'semicolon)
       (loop stmts)]
      [_
       (loop (cons (parse-statement l) stmts))])))

(define (parse-statement l)
  (match (lexer-peek l)
    [(or (name _) (lparen))
     (parse-assignment-or-call l)]
    [(coloncolon)
     (define ctxt (token-ctxt (expect l 'coloncolon)))
     (define name (parse-name l))
     (begin0 (Label ctxt name)
       (skip l 'coloncolon))]
    [(keyword 'break)
     (Break (token-ctxt (expect l 'keyword 'break)))]
    [(keyword 'goto)
     (define ctxt (token-ctxt (expect l 'keyword 'goto)))
     (define label (parse-name l))
     (Goto ctxt label)]
    [(keyword 'do)
     (define ctxt (token-ctxt (expect l 'keyword 'do)))
     (define block (parse-block l))
     (begin0 (Do ctxt block)
       (skip l 'keyword 'end))]
    [(keyword 'while)
     (define ctxt (token-ctxt (expect l 'keyword 'while)))
     (define cond-expr (parse-expr l))
     (skip l 'keyword 'do)
     (define block (parse-block l))
     (begin0 (While ctxt cond-expr block)
       (skip l 'keyword 'end))]
    [(keyword 'repeat)
     (define ctxt (token-ctxt (expect l 'keyword 'repeat)))
     (define block (parse-block l '(until)))
     (skip l 'keyword 'until)
     (define cond-expr (parse-expr l))
     (Repeat ctxt cond-expr block)]
    [(keyword 'if)
     (parse-if-block l)]
    [(keyword 'function)
     (define ctxt (token-ctxt (expect l 'keyword 'function)))
     (define function
       (match (parse-funcname l)
         [(? symbol? name)
          (FuncDef ctxt name (parse-params l) (parse-block l))]
         [(? list? names)
          (FuncDef ctxt names (parse-params l) (parse-block l))]
         [(cons names attr)
          (MethodDef ctxt names attr (parse-params l) (parse-block l))]))
     (begin0 function
       (skip l 'keyword 'end))]
    [(keyword 'for)
     (parse-for-block l)]
    [(keyword 'local)
     (define ctxt (token-ctxt (expect l 'keyword 'local)))
     (match (lexer-peek l)
       [(keyword 'function)
        (skip l 'keyword 'function)
        (define name (parse-name l))
        (define params (parse-params l))
        (define block (parse-block l))
        (begin0 (LocalFunction ctxt name params block)
          (expect l 'keyword 'end))]
       [_
        (define names (parse-names l))
        (match (lexer-peek l)
          [(op '=)
           (skip l 'op '=)
           (define exprs (parse-exprs l))
           (LocalAssignment ctxt names exprs)]
          [_
           (LocalAssignment ctxt names null)])])]
    [tok
     (expected "statement" tok)]))

(define (parse-for-block l)
  (define tok (expect l 'keyword 'for))
  (define ctxt (token-ctxt tok))
  (define name (parse-name l))
  (match (lexer-peek l)
    [(op '=)
     (skip l 'op '=)
     (define init-expr
       (parse-expr l))
     (skip l 'comma)
     (define limit-expr
       (parse-expr l))
     (define step-expr
       (match (lexer-peek l)
         [(comma)
          (skip l 'comma)
          (parse-expr l)]
         [_ 1]))
     (skip l 'keyword 'do)
     (define block
       (parse-block l))
     (begin0 (For ctxt name init-expr limit-expr step-expr block)
       (skip l 'keyword 'end))]
    [(comma)
     (skip l 'comma)
     (define names (cons name (parse-names l)))
     (skip l 'keyword 'in)
     (define exprs (parse-exprs l))
     (skip l 'keyword 'do)
     (define block
       (parse-block l))
     (begin0 (ForIn ctxt names exprs block)
       (skip l 'keyword 'end))]
    [(keyword 'in)
     (define names (list name))
     (skip l 'keyword 'in)
     (define exprs (parse-exprs l))
     (skip l 'keyword 'do)
     (define block
       (parse-block l))
     (begin0 (ForIn ctxt names exprs block)
       (skip l 'keyword 'end))]
    [_
     (expected "equals, comma or 'in' keyword" tok)]))

(define (parse-if-block l [kwd 'if])
  (define ctxt (token-ctxt (expect l 'keyword kwd)))
  (define cond-expr (parse-expr l))
  (skip l 'keyword 'then)
  (define then-block (parse-block l '(elseif else end)))
  (define else-block
    (match (lexer-peek l)
      [(keyword 'elseif)
       (parse-if-block l 'elseif)]
      [(keyword 'else)
       (skip l 'keyword 'else)
       (parse-block l)]
      [_
       #f]))
  (begin0 (If ctxt cond-expr then-block else-block)
    (when (eq? kwd 'if)
      (skip l 'keyword 'end))))

(define (parse-assignment-or-call l)
  (let step ([vars null]
             [exps null])
    (match (parse-primaryexpr l)
      [(? Call? e) e]
      [(? CallMethod? e) e]
      [e (match (lexer-peek l)
           [(comma)
            (skip l 'comma)
            (step (cons e vars) exps)]
           [(and (op '=) tok)
            (skip l 'op)
            (Assignment
             (token-ctxt tok)
             (reverse (cons e vars))
             (parse-exprs l))]
           [tok
            (expected "assignment" tok)])])))

(define (parse-primaryexpr l [e (parse-prefixexp l)])
  (define tok
    (lexer-peek l))
  (match tok
    [(dot)
     (skip l 'dot)
     (parse-primaryexpr l (Attribute (token-ctxt tok) e (parse-name l)))]
    [(lsqbrace)
     (skip l 'lsqbrace)
     (define sub-e (parse-expr l))
     (skip l 'rsqbrace)
     (parse-primaryexpr l (Subscript (token-ctxt tok) e sub-e))]
    [(lparen)
     (parse-primaryexpr l (Call (token-ctxt tok) e (parse-args l)))]
    [(colon)
     (skip l 'colon)
     (define name (parse-name l))
     (define args (parse-args l))
     (parse-primaryexpr l (CallMethod (token-ctxt tok) e name args))]
    [_ e]))

(define (parse-prefixexp l)
  (match (lexer-peek l)
    [(name _) (parse-name l)]
    [(lparen) (parse-expr l)]
    [tok (expected "prefixexp" tok)]))

(define (parse-exprs l)
  (let loop ([exprs null])
    (define expr
      (parse-expr l))
    (match (lexer-peek l)
      [(comma)
       (skip l 'comma)
       (loop (cons expr exprs))]
      [_
       (reverse (cons expr exprs))])))

(define (parse-expr l [lhs-e (parse-term l)] [depth 0])
  (let step ([lhs-e lhs-e]
             [depth depth])
    (match (lexer-peek l)
      [(binop current-prec)
       #:when (>= current-prec depth)
       (define tok (lexer-take l))
       (let loop ([rhs-e (parse-term l)])
         (define next-tok (lexer-peek l))
         (match next-tok
           [(binop next-prec)
            #:when (or (> next-prec current-prec)
                       (and (= next-prec current-prec)
                            (right-associative? (token-val next-tok))))
            (loop (step rhs-e next-prec))]

           [_
            (step (Binop (token-ctxt tok) (token-val tok) lhs-e rhs-e) depth)]))]
      [_ lhs-e])))

(define (parse-term l)
  (match (lexer-peek l)
    [(keyword 'nil)
     (begin0 'nil
       (skip l 'keyword))]
    [(keyword 'false)
     (begin0 #f
       (skip l 'keyword))]
    [(keyword 'true)
     (begin0 #t
       (skip l 'keyword))]
    [(number n)
     (begin0 n
       (skip l 'number))]
    [(string s)
     (begin0 s
       (skip l 'string))]
    [(name _)
     (parse-primaryexpr l)]
    [(and (dotdotdot) tok)
     (define ctxt (token-ctxt tok))
     (begin0 (Call ctxt '#%va-args null)
       (skip l 'dotdotdot))]
    [(op (and (or 'not '\# '~ '-) id))
     (parse-unary-expr l id)]
    [(keyword 'function)
     (parse-function l)]
    [(lparen)
     (skip l 'lparen)
     (begin0 (parse-expr l)
       (skip l 'rparen))]
    [(lcubrace)
     (parse-table l)]
    [tok
     (expected "expression" tok)]))

(define (parse-unary-expr l id)
  (define op
    (case id
      [(not) 'not]
      [(\#) '#%length]
      [(~) '#%bitwise-not]
      [(-) '#%unary-minus]
      [else (error 'parse-unary-expr "bad id: ~a" id)]))
  (define tok (expect l 'op id))
  (define prec (precedence op))
  (define expr (parse-expr l (parse-term l) prec))
  (Unop (token-ctxt tok) op expr))

(define (parse-args l)
  (match (lexer-peek l)
    [(rparen)
     (begin0 null
       (skip ))]
    [(lparen)
     (skip l 'lparen)
     (match (lexer-peek l)
       [(rparen)
        (begin0 null
          (skip l 'rparen))]
       [_
        (begin0 (parse-exprs l)
          (skip l 'rparen))])]
    [(lcubrace)
     (parse-table l)]
    [(string s)
     s]
    [tok
     (expected "args" tok)]))

(define (parse-function l)
  (define function
    (Func
     (token-ctxt (expect l 'keyword 'function))
     (parse-params l)
     (parse-block l)))
  (begin0 function
    (expect l 'keyword 'end)))

(define (parse-params l)
  (expect l 'lparen)
  (define params
    (match (lexer-peek l)
      [(rparen) null]
      [_ (parse-names l #t)]))
  (begin0 params
    (expect l 'rparen)))

(define (parse-names l [vararg? #f])
  (let loop ([names null])
    (match (lexer-peek l)
      [(dotdotdot)
       #:when vararg?
       (skip l 'dotdotdot)
       (reverse (cons '... names))]
      [_
       (define name
         (parse-name l))
       (match (lexer-peek l)
         [(comma)
          (skip l 'comma)
          (loop (cons name names))]
         [_
          (reverse (cons name names))])])))

(define (parse-table l)
  (define table
    (Table
     (token-ctxt (expect l 'lcubrace))
     (match (lexer-peek l)
       [(rcubrace) null]
       [_ (parse-fields l)])))
  (begin0 table
    (skip l 'rcubrace)))

(define (parse-fields l)
  (reverse
   (let loop ([fields null])
     (define field
       (parse-field l))
     (match (lexer-peek l)
       [(comma)
        (skip l 'comma)
        (loop (cons field fields))]
       [(semicolon)
        (skip l 'semicolon)
        (loop (cons field fields))]
       [_
        (cons field fields)]))))

(define (parse-field l)
  (match (lexer-peek l)
    [(lsqbrace)
     (define ctxt (token-ctxt (expect l 'lsqbrace)))
     (define field-expr (parse-expr l))
     (skip l 'rsqbrace)
     (skip l 'op '=)
     (define value-expr (parse-expr l))
     (FieldExpr ctxt field-expr value-expr)]
    [(and (name _) tok)
     (define ctxt (token-ctxt tok))
     (define name-or-expr (parse-primaryexpr l))
     (match (lexer-peek l)
       [(op '=)
        #:when (symbol? name-or-expr)
        (define name name-or-expr)
        (skip l 'op '=)
        (define expr (parse-expr l))
        (FieldLit ctxt name expr)]
       [_
        (Field ctxt name-or-expr)])]
    [tok
     (Field
      (token-ctxt tok)
      (parse-expr l))]))

(define (parse-funcname l)
  (let loop ([names null])
    (define name
      (parse-name l))
    (match (lexer-peek l)
      [(dot)
       (skip l 'dot)
       (loop (cons name names))]
      [(colon)
       (skip l 'colon)
       (define attrib (reverse (cons name names)))
       (define method (parse-name l))
       (cons attrib method)]
      [_ #:when (null? names) name]
      [_ (reverse (cons name names))])))

(define (parse-name l)
  (token-val (expect l 'name)))


;; helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (raise-parse-error t message)
  (raise-read-error
   message
   (current-source-name)
   (token-line t)
   (token-col t)
   (token-pos t)
   #f))

(define (expected what tok [accessor token-str])
  (define message
    (format "expected ~a but found ~a" what (accessor tok)))
  (raise-parse-error tok message))

(define (expect l type [val #f])
  (define t (lexer-take l))
  (begin0 t
    (unless (eq? (token-type t) type)
      (expected (or val type) t))
    (when (and val (not (equal? (token-val t) val)))
      (expected val t token-val))))

(define (skip . args)
  (void (apply expect args)))

(define (maybe-skip l type)
  (when (eq? (token-type (lexer-peek l)) type)
    (skip l type)))

(define (token-ctxt t)
  (define str (token-str t))
  (define loc
    (vector
     (current-source-name)
     (token-line t)
     (token-col t)
     (token-pos t)
     (and (string? str)
          (string-length str))))
  (datum->syntax #f 'srcloc loc))
