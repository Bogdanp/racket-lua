#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/format
         racket/list
         racket/match
         racket/string
         racket/syntax
         "ast.rkt")

(provide
 compile-chunk)

(define (compile-chunk chunk)
  (define loc (Node-loc chunk))
  (define return? (needs-return? chunk))
  (with-syntax ([block (compile-block chunk)])
    (with-syntax ([body (if return?
                            (syntax/loc* loc (#%let/cc #%return block (#%values)))
                            (syntax/loc* loc (#%begin block (#%values))))])
      (syntax/loc* loc
        ((#%provide #%chunk)
         (#%define _ENV (#%global))
         (#%load-stdlib! _ENV)
         (#%define (#%lua-module . #%rest)
           body)
         (#%define #%chunk
           (#%adjust (#%lua-module))))))))

(define/match (compile-block _)
  [((Block loc stmts))
   (match (Lua->L1 stmts)
     [(list)
      (syntax/loc* loc (#%values))]
     [stmts
      (with-syntax ([(statement ...) (map compile-statement stmts)])
        (syntax/loc* loc
          (#%begin statement ...)))])])

(define/match (compile-statement e)
  [((Assignment loc vars (list exprs ... (vararg vararg-expr))))
   (with-syntax ([((attr-temp attr-exp) ...)
                  (vars->attr-temp&exprs vars)]
                 [((sub-lhs-temp sub-lhs-expr sub-rhs-temp sub-rhs-expr) ...)
                  (vars->sub-temp&exprs vars)]
                 [((temp var expr) ...)
                  (for/list ([idx (in-naturals)]
                             [var (in-list vars)]
                             [expr (in-list exprs)])
                    (list (format-id #f "#%temp~a" idx)
                          (compile-assignment-var var idx)
                          (compile-expr* expr)))]
                 [vararg-expr (compile-expr vararg-expr)]
                 [((va-temp va-var va-idx) ...)
                  (let ([start (min (length vars) (length exprs))])
                    (for/list ([tmp-idx (in-naturals start)]
                               [tbl-idx (in-naturals)]
                               [var (in-list (drop vars start))])
                      (list (format-id #f "#%temp~a" tmp-idx)
                            (compile-assignment-var var tmp-idx)
                            (add1 tbl-idx))))])
     (syntax/loc* loc
       (#%let
         ([attr-temp attr-exp]
          ...
          [sub-lhs-temp sub-lhs-expr]
          ...
          [sub-rhs-temp sub-rhs-expr]
          ...
          [temp expr]
          ...
          [#%t (#%apply #%table (#%adjust-va vararg-expr))])
         (#%let
           ([va-temp (#%subscript #%t va-idx)] ...)
           (#%set! var temp) ...
           (#%set! va-var va-temp) ...))))]

  [((Assignment loc vars exprs))
   (let ([exprs (indexed exprs)])
     (with-syntax ([((attr-temp attr-expr) ...)
                    (vars->attr-temp&exprs vars)]
                   [((sub-lhs-temp sub-lhs-expr sub-rhs-temp sub-rhs-expr) ...)
                    (vars->sub-temp&exprs vars)]
                   [((temp var expr) ...)
                    (for/list ([idx (in-naturals)]
                               [var (in-list vars)])
                      (define temp (format-id #f "#%temp~a" idx))
                      (define expr (hash-ref exprs idx 'nil))
                      (list temp
                            (compile-assignment-var var idx)
                            (compile-expr* expr)))])
       (syntax/loc* loc
         (#%let
           ([attr-temp attr-expr]
            ...
            [sub-lhs-temp sub-lhs-expr]
            ...
            [sub-rhs-temp sub-rhs-expr]
            ...
            [temp expr]
            ...)
           (#%set! var temp) ...))))]

  [((Break loc))
   (syntax/loc* loc
     (#%break))]

  [((or (? Call?)
        (? CallMethod?)))
   (compile-call e)]

  [((Do loc block))
   (with-syntax ([block (compile-block block)])
     (syntax/loc* loc
       (#%let () block)))]

  [((For loc name init-expr limit-expr step-expr block))
   (define break? (needs-break? block))
   (with-syntax ([name (compile-expr name)]
                 [init-expr (compile-expr init-expr)]
                 [limit-expr (compile-expr limit-expr)]
                 [step-expr (compile-expr step-expr)]
                 [block (compile-block block)])
     (with-syntax ([loop
                    (syntax/loc* loc
                      (#%let
                        ([#%init init-expr]
                         [#%limit limit-expr]
                         [#%step step-expr])
                        (#%let #%for ([name #%init])
                          (#%when
                            (#%cond
                              [(< #%step 0) (>= name #%limit)]
                              [(> #%step 0) (<= name #%limit)]
                              [#%else (#%error "for: zero step")])
                            block
                            (#%for (+ name #%step))))))])
       (if break?
           (syntax/loc* loc (#%let/cc #%break loop))
           (syntax/loc* loc loop))))]

  [((ForIn loc names exprs block))
   (define protect-stmt
     (Protect
      loc
      (list
       (While
        loc #t
        (Block
         loc
         (list*
          (Assignment loc names (list (Call loc '#%iter '(#%state #%control))))
          (Assignment loc '(#%control) (list (car names)))
          (If loc
              (Binop loc '== '#%control 'nil)
              (Block loc (list (Break loc))) #f)
          (Block-stmts block)))))
      (list
       (If loc
           (Binop loc '~= '#%closing 'nil)
           (Block loc (list (Call loc '#%closing null)))
           #f))))
   (compile-statement
    (Let loc '(#%iter #%state #%control #%closing) exprs (list protect-stmt)))]

  [((FuncDef loc (? list? names) params block))
   (parameterize ([current-procedure-name (names->procedure-name names)])
     (compile-statement
      (Assignment loc
                  (list (names->subscripts loc names))
                  (list (Func loc params block)))))]

  [((FuncDef loc name params block))
   (parameterize ([current-procedure-name (compile-expr name)])
     (compile-statement
      (Assignment loc
                  (list name)
                  (list (Func loc params block)))))]

  [((Goto loc (Name name-loc name)))
   (with-syntax ([name (format-label-id name-loc name)])
     (syntax/loc* loc
       (name name)))]

  [((If loc cond-expr then-block #f))
   (with-syntax ([cond-expr (compile-expr* cond-expr)]
                 [then-block (compile-block then-block)])
     (syntax/loc* loc
       (#%when cond-expr then-block)))]

  [((If loc cond-expr then-block (? If? elseif-block)))
   (with-syntax ([cond-expr (compile-expr* cond-expr)]
                 [then-block (compile-block then-block)]
                 [else-block (compile-statement elseif-block)])
     (syntax/loc* loc
       (#%cond
         [cond-expr then-block nil]
         [#%else else-block nil])))]

  [((If loc cond-expr then-block else-block))
   (with-syntax ([cond-expr (compile-expr* cond-expr)]
                 [then-block (compile-block then-block)]
                 [else-block (compile-block else-block)])
     (syntax/loc* loc
       (#%cond
         [cond-expr then-block nil]
         [#%else else-block nil])))]

  [((Label loc (Name name-loc name)))
   (with-syntax ([name (format-label-id name-loc name)])
     (syntax/loc* loc
       (#%define name (#%call/cc #%values))))]

  [((Let loc vars (list exprs ... (vararg vararg-expr)) stmts))
   (with-syntax ([((temp var expr) ...)
                  (for/list ([idx (in-naturals)]
                             [var (in-list vars)]
                             [expr (in-list exprs)])
                    (list (format-id #f "#%temp~a" idx)
                          (compile-expr var)
                          (compile-expr* expr)))]
                 [vararg-expr (compile-expr vararg-expr)]
                 [((va-temp va-var va-idx) ...)
                  (let ([start (min (length vars) (length exprs))])
                    (for/list ([tmp-idx (in-naturals start)]
                               [tbl-idx (in-naturals)]
                               [var (in-list (drop vars start))])
                      (list (format-id #f "#%temp~a" tmp-idx)
                            (compile-expr var)
                            (add1 tbl-idx))))]
                 [(stmt ...) (maybe-void (map compile-statement stmts))])
     (syntax/loc* loc
       (#%let
         ([temp expr]
          ...
          [#%t (#%apply #%table (#%adjust-va vararg-expr))])
         (#%let
           ([va-temp (#%subscript #%t va-idx)] ...)
           (#%let
             ([var temp]
              ...
              [va-var va-temp]
              ...)
             stmt ...)))))]

  [((Let loc vars exprs stmts))
   (let ([exprs (indexed exprs)])
     (with-syntax ([((temp var expr) ...)
                    (for/list ([idx (in-naturals)]
                               [var (in-list vars)])
                      (define temp (format-id #f "#%temp~a" idx))
                      (define expr (hash-ref exprs idx 'nil))
                      (list temp
                            (compile-expr var)
                            (compile-expr* expr)))]
                   [(stmt ...) (maybe-void (map compile-statement stmts))])
       (syntax/loc* loc
         (#%let
           ([temp expr] ...)
           (#%let
             ([var temp] ...)
             stmt ...)))))]

  [((LetFunction loc name params block stmts))
   (with-syntax ([name (compile-expr name)]
                 [func-expr (compile-expr (Func loc params block))]
                 [(stmt ...) (maybe-void (map compile-statement stmts))])
     (syntax/loc* loc
       (#%letrec ([name func-expr])
         stmt ...)))]

  [((MethodDef loc names (Name _ attr) params block))
   (parameterize ([current-procedure-name (names->method-name names attr)])
     (compile-statement
      (Assignment loc
                  (list (Subscript loc
                                   (names->subscripts loc names)
                                   (symbol->bytes attr)))
                  (list (Func loc (cons 'self params) block)))))]

  [((Protect loc value-stmts post-stmts))
   (with-syntax ([(value-stmt ...) (maybe-void (map compile-statement value-stmts))]
                 [(post-stmt ...) (maybe-void (map compile-statement post-stmts))])
     (syntax/loc* loc
       (#%dynamic-wind
         #%void
         (#%lambda () value-stmt ...)
         (#%lambda () post-stmt ...))))]

  [((Repeat loc cond-expr block))
   (define break? (needs-break? block))
   (with-syntax ([cond-expr (compile-expr cond-expr)]
                 [block (compile-block block)])
     (with-syntax ([loop
                    (syntax/loc* loc
                      (#%let #%repeat ()
                        block
                        (#%unless cond-expr
                          (#%repeat))))])
       (if break?
           (syntax/loc* loc (#%let/cc #%break loop))
           (syntax/loc* loc loop))))]

  [((Return loc (list exprs ... (vararg vararg-expr))))
   (with-syntax ([(expr ...) (map compile-expr* exprs)]
                 [vararg-expr (compile-expr vararg-expr)])
     (syntax/loc* loc
       (#%apply #%return expr ... (#%adjust-va vararg-expr))))]

  [((Return loc exprs))
   (with-syntax ([(expr ...) (map compile-expr* exprs)])
     (syntax/loc* loc
       (#%return expr ...)))]

  [((While loc cond-expr block))
   (define break? (needs-break? block))
   (with-syntax ([cond-expr (compile-expr cond-expr)]
                 [block (compile-block block)])
     (with-syntax ([loop
                    (syntax/loc* loc
                      (#%let #%while ()
                        (#%when cond-expr
                          block
                          (#%while))))])
       (if break?
           (syntax/loc* loc (#%let/cc #%break loop))
           (syntax/loc* loc loop))))])

(define/match (compile-expr* e)
  [((or (Call loc _ _)
        (CallMethod loc _ _ _)))
   (with-syntax ([expr (compile-expr e)])
     (syntax/loc* loc
       (#%adjust expr)))]

  [(_)
   (compile-expr e)])

(define/match (compile-expr e)
  [((? boolean?)) (datum->syntax #f e)]
  [((? number?))  (datum->syntax #f e)]
  [((? bytes?))   (datum->syntax #f e)]
  [((? symbol?))  (datum->syntax #f e)]

  [((Attribute loc expr (Name _ name)))
   (with-syntax ([expr (compile-expr* expr)]
                 [name (symbol->bytes name)])
     (syntax/loc* loc
       (#%subscript expr name)))]

  [((Binop loc op lhs-expr rhs-expr))
   (with-syntax ([binop (compile-expr op)]
                 [lhs-expr (compile-expr* lhs-expr)]
                 [rhs-expr (compile-expr* rhs-expr)])
     (syntax/loc* loc
       (binop lhs-expr rhs-expr)))]

  [((or (? Call?) (? CallMethod?)))
   (compile-call e)]

  [((Func loc (list params ... '...) block))
   (define return? (needs-return? block))
   (with-syntax ([procedure-name (current-procedure-name)]
                 [(param ...) (map compile-expr params)]
                 [block (compile-block block)])
     (with-syntax ([body (if return?
                             (syntax/loc* loc (#%let/cc #%return block (#%values)))
                             (syntax/loc* loc (#%begin block (#%values))))])
       (syntax/loc* loc
         (#%procedure-rename
          (#%lambda ([param nil] ... . #%rest)
            body)
          procedure-name))))]

  [((Func loc params block))
   (define return? (needs-return? block))
   (with-syntax ([procedure-name (current-procedure-name)]
                 [(param ...) (map compile-expr params)]
                 [block (compile-block block)])
     (with-syntax ([body (if return?
                             (syntax/loc* loc (#%let/cc #%return block (#%values)))
                             (syntax/loc* loc (#%begin block (#%values))))])
       (syntax/loc* loc
         (#%procedure-rename
          (#%lambda ([param nil] ... . #%unused-rest)
            body)
          procedure-name))))]

  [((Name loc symbol))
   (datum->syntax #f symbol loc (get-original-stx))]

  [((Subscript loc expr field-expr))
   (with-syntax ([expr (compile-expr* expr)]
                 [field-expr (compile-expr* field-expr)])
     (syntax/loc* loc
       (#%subscript expr field-expr)))]

  [((Table loc (list field-exprs ... (Field _ (vararg vararg-expr)))))
   (with-syntax ([(field-expr ...) (map compile-field field-exprs)]
                 [vararg-expr (compile-expr vararg-expr)])
     (syntax/loc* loc
       (#%apply #%table field-expr ... (#%adjust-va vararg-expr))))]

  [((Table loc field-exprs))
   (with-syntax ([(field-expr ...) (map compile-field field-exprs)])
     (syntax/loc* loc
       (#%table field-expr ...)))]

  [((Unop loc op expr))
   (with-syntax ([unop (compile-expr op)]
                 [expr (compile-expr* expr)])
     (syntax/loc* loc
       (unop expr)))])

(define/match (compile-call _e)
  [((CallMethod loc target-expr (Name _ attr) arg-exprs))
   (define subscript-expr (Subscript loc '#%instance (symbol->bytes attr)))
   (with-syntax ([target-expr (compile-expr* target-expr)]
                 [call-expr (compile-expr (Call loc subscript-expr (cons '#%instance arg-exprs)))])
     (syntax/loc* loc
       (#%let ([#%instance target-expr]) call-expr)))]

  [((Call loc rator-expr (list rand-exprs ... (vararg vararg-expr))))
   (with-syntax ([rator-expr (compile-expr* rator-expr)]
                 [(rand-expr ...) (map compile-expr* rand-exprs)]
                 [vararg-expr (compile-expr vararg-expr)])
     (syntax/loc* loc
       (#%apply rator-expr rand-expr ... (#%adjust-va vararg-expr))))]

  [((Call loc rator-expr rand-exprs))
   (with-syntax ([rator-expr (compile-expr* rator-expr)]
                 [(rand-expr ...) (map compile-expr* rand-exprs)])
     (syntax/loc* loc
       (rator-expr rand-expr ...)))])

(define/match (compile-field _e)
  [((Field loc expr))
   (with-syntax ([expr (compile-expr* expr)])
     (syntax/loc* loc
       expr))]

  [((FieldExpr loc field-expr value-expr))
   (with-syntax ([field-expr (compile-expr* field-expr)]
                 [value-expr (compile-expr* value-expr)])
     (syntax/loc* loc
       (#%cons field-expr value-expr)))]

  [((FieldLit loc (Name _ name) expr))
   (with-syntax ([name (symbol->bytes name)]
                 [expr (compile-expr* expr)])
     (syntax/loc* loc
       (#%cons name expr)))])


;; passes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lua: The standard lua language.
;; L1: Removes Local{Assignment, Function}, adds Let

(define (Lua->L1 stmts)
  (let loop ([res null]
             [stmts stmts])
    (match stmts
      [(list)
       (reverse res)]
      [(cons (LocalAssignment loc names exprs) stmts)
       (define node (Let loc names exprs (Lua->L1 stmts)))
       (reverse (cons node res))]
      [(cons (LocalFunction loc name params block) stmts)
       (define node (LetFunction loc name params block (Lua->L1 stmts)))
       (reverse (cons node res))]
      [(cons stmt stmts)
       (loop (cons stmt res) stmts)])))


;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-match-expander vararg
  (lambda (stx)
    (syntax-parse stx
      [(_ e) #'(and (or (? Call?) (? CallMethod?) '#%rest) e)])))

(define symbol->bytes
  (compose1 string->bytes/utf-8 symbol->string))

(define (format-label-id loc id)
  (format-id #f "#%label:~a" id #:source loc))

(define (names->subscripts loc names)
  (let loop ([target (car names)]
             [names (cdr names)])
    (cond
      [(null? names) target]
      [else
       (define sub (Subscript loc target (symbol->bytes (Name-symbol (car names)))))
       (loop sub (cdr names))])))

(define (indexed lst)
  (for/hasheqv ([idx (in-naturals)]
                [val (in-list lst)])
    (values idx val)))

(define (vars->attr-temp&exprs vars)
  (for/list ([idx (in-naturals)]
             [var (in-list vars)]
             #:when (Attribute? var))
    (list (format-id #f "#%attr-temp~a" idx)
          (compile-expr* (Attribute-e var)))))

(define (vars->sub-temp&exprs vars)
  (for/list ([idx (in-naturals)]
             [var (in-list vars)]
             #:when (Subscript? var))
    (list (format-id #f "#%sub-lhs-temp~a" idx)
          (compile-expr* (Subscript-e var))
          (format-id #f "#%sub-rhs-temp~a" idx)
          (compile-expr* (Subscript-sub-e var)))))

(define (compile-assignment-var var idx)
  (compile-expr
   (match var
     [(Attribute loc _ name)
      (define temp (format-sym "#%attr-temp~a" idx))
      (Attribute loc temp name)]
     [(Subscript loc _ _)
      (define lhs-temp (format-sym "#%sub-lhs-temp~a" idx))
      (define rhs-temp (format-sym "#%sub-rhs-temp~a" idx))
      (Subscript loc lhs-temp rhs-temp)]
     [_
      var])))

(define (format-sym fmt . args)
  (string->symbol (apply format fmt args)))

(define (maybe-void stmts)
  (if (null? stmts) '((#%void)) stmts))

(define ((make-statement-walker base-proc [enter-loops? #t]) e)
  (let loop ([e e])
    (match e
      [(Block _ stmts)
       (ormap loop stmts)]
      [(Do _ block)
       (loop block)]
      [(For _ _ _ _ _ block)
       #:when enter-loops?
       (loop block)]
      [(If _ _ then-block #f)
       (loop then-block)]
      [(If _ _ then-block else-block)
       (or (loop then-block)
           (loop else-block))]
      [(Let _ _ _ stmts)
       (ormap loop stmts)]
      [(Repeat _ _ block)
       #:when enter-loops?
       (loop block)]
      [(While _ _ block)
       #:when enter-loops?
       (loop block)]
      [_
       (base-proc e)])))

(define needs-break?
  (make-statement-walker Break? #f))
(define needs-return?
  (make-statement-walker Return?))


;; procedure names ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-procedure-name
  (make-parameter 'anon))

(define (names->procedure-name names)
  (string->symbol
   (string-join
    (for/list ([name (in-list names)])
      (symbol->string (Name-symbol name)))
    ".")))

(define (names->method-name names attr)
  (string->symbol (~a (names->procedure-name names) ":" attr)))


;; stxlocs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (syntax/loc* stx)
  (syntax-parse stx
    [(_ loc:expr form)
     #'(replace-srcloc #'here loc #'form)]))

(define (replace-srcloc where-stx with-loc stx)
  (cond
    [(list? stx)
     (for/list ([child-stx (in-list stx)])
       (replace-srcloc where-stx with-loc child-stx))]
    [(and (syntax? stx)
          (equal? (syntax-source where-stx)
                  (syntax-source stx)))
     (define content (replace-srcloc where-stx with-loc (syntax-e stx)))
     (datum->syntax #f content with-loc stx)]
    [else
     stx]))

;; The 'original property on syntax is not preserved in compiled code.
;; This procedure works around that problem by calling `read-syntax'
;; at runtime for every newly-compiled module.
(define get-original-stx
  (let ([stx #f])
    (lambda ()
      (unless stx
        (set! stx (read-syntax "<compiler>" (open-input-string "id"))))
      stx)))
