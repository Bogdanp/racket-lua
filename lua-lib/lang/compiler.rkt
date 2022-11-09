#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/list
         racket/match
         racket/syntax
         "ast.rkt")

(provide
 compile-chunk)

(define (compile-chunk chunk)
  (with-syntax ([block (compile-block chunk)])
    #'((#%provide #%chunk)
       (#%define _ENV (#%global))
       (#%define (#%chunk-proc . #%rest) (#%let/ec #%return block))
       (#%define #%chunk (#%adjust (#%chunk-proc))))))

(define/match (compile-block _)
  [((Block ctxt stmts))
   (with-syntax ([(statement ...) (map compile-statement (Lua->L1 stmts))])
     (syntax/loc ctxt
       (#%begin statement ... (#%values))))])

(define/match (compile-statement e)
  [((Assignment ctxt vars (list exprs ... (vararg vararg-expr))))
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
                    (for/list ([idx (in-naturals start)]
                               [var (in-list (drop vars start))])
                      (list (format-id #f "#%temp~a" idx)
                            (compile-assignment-var var idx)
                            (add1 idx))))])
     (syntax/loc ctxt
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
           ([va-temp (#%table-ref #%t va-idx)] ...)
           (#%set! var temp) ...
           (#%set! va-var va-temp) ...))))]

  [((Assignment ctxt vars exprs))
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
       (syntax/loc ctxt
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

  [((Break ctxt))
   (syntax/loc ctxt
     (#%break))]

  [((or (? Call?)
        (? CallMethod?)))
   (compile-call e)]

  [((Do ctxt block))
   (with-syntax ([block (compile-block block)])
     (syntax/loc ctxt
       (#%let () block)))]

  [((For ctxt name init-expr limit-expr step-expr block))
   (with-syntax ([name name]
                 [init-expr (compile-expr init-expr)]
                 [limit-expr (compile-expr limit-expr)]
                 [step-expr (compile-expr step-expr)]
                 [block (compile-block block)])
     (syntax/loc ctxt
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
             (#%for (+ name #%step)))))))]

  [((ForIn ctxt names exprs block))
   (define protect-stmt
     (Protect
      ctxt
      (list
       (While
        ctxt #t
        (Block
         ctxt
         (list*
          (Assignment ctxt names (list (Call ctxt '#%iter '(#%state #%control))))
          (Assignment ctxt '(#%control) (list (car names)))
          (If ctxt
              (Binop ctxt '== '#%control 'nil)
              (Block ctxt (list (Break ctxt))) #f)
          (Block-stmts block)))))
      (list
       (If ctxt
           (Binop ctxt '~= '#%closing 'nil)
           (Block ctxt (list (Call ctxt '#%closing null)))
           #f))))
   (compile-statement
    (Let ctxt '(#%iter #%state #%control #%closing) exprs (list protect-stmt)))]

  [((FuncDef ctxt (? list? names) params block))
   (compile-statement
    (Assignment ctxt
                (list (names->subscripts ctxt names))
                (list (Func ctxt params block))))]

  [((FuncDef ctxt name (list params ... '...) block))
   (with-syntax ([name name]
                 [(param ...) params]
                 [block (compile-block block)])
     (syntax/loc ctxt
       (#%define (name [param nil] ... . #%rest) (#%let/ec #%return block))))]

  [((FuncDef ctxt name params block))
   (with-syntax ([name name]
                 [(param ...) params]
                 [block (compile-block block)])
     (syntax/loc ctxt
       (#%define (name [param nil] ... . #%unused-rest) (#%let/ec #%return block))))]

  ;; FIXME: goto needs to be able to jump forwards.
  [((Goto ctxt name))
   (with-syntax ([name (format-label-id name)])
     (syntax/loc ctxt
       (name name)))]

  [((If ctxt cond-expr then-block #f))
   (with-syntax ([cond-expr (compile-expr* cond-expr)]
                 [then-block (compile-block then-block)])
     (syntax/loc ctxt
       (#%when cond-expr then-block)))]

  [((If ctxt cond-expr then-block (? If? elseif-block)))
   (with-syntax ([cond-expr (compile-expr* cond-expr)]
                 [then-block (compile-block then-block)]
                 [else-block (compile-statement elseif-block)])
     (syntax/loc ctxt
       (#%cond
         [cond-expr then-block nil]
         [#%else else-block nil])))]

  [((If ctxt cond-expr then-block else-block))
   (with-syntax ([cond-expr (compile-expr* cond-expr)]
                 [then-block (compile-block then-block)]
                 [else-block (compile-block else-block)])
     (syntax/loc ctxt
       (#%cond
         [cond-expr then-block nil]
         [#%else else-block nil])))]

  [((Label ctxt name))
   (with-syntax ([name (format-label-id name)])
     (syntax/loc ctxt
       (#%define name (#%call/cc #%values))))]

  [((Let ctxt vars (list exprs ... (vararg vararg-expr)) stmts))
   (with-syntax ([((temp var expr) ...)
                  (for/list ([idx (in-naturals)]
                             [var (in-list vars)]
                             [expr (in-list exprs)])
                    (define temp (format-id #f "#%temp~a" idx))
                    (list temp var (compile-expr* expr)))]
                 [vararg-expr (compile-expr vararg-expr)]
                 [((va-var va-temp va-idx) ...)
                  (let ([start (min (length vars) (length exprs))])
                    (for/list ([idx (in-naturals start)]
                               [var (in-list (drop vars start))])
                      (define temp (format-id #f "#%temp~a" idx))
                      (list var temp (add1 idx))))]
                 [(stmt ...) (map compile-statement stmts)])
     (syntax/loc ctxt
       (#%let
         ([temp expr]
          ...
          [#%t (#%apply #%table (#%adjust-va vararg-expr))])
         (#%let
           ([va-temp (#%table-ref #%t va-idx)] ...)
           (#%let
             ([var temp]
              ...
              [va-var va-temp]
              ...)
             stmt ...
             (#%void))))))]

  [((Let ctxt vars exprs stmts))
   (let ([exprs (indexed exprs)])
     (with-syntax ([((temp var expr) ...)
                    (for/list ([idx (in-naturals)]
                               [var (in-list vars)])
                      (define temp (format-id #f "#%temp~a" idx))
                      (define expr (hash-ref exprs idx 'nil))
                      (list temp var (compile-expr* expr)))]
                   [(stmt ...) (map compile-statement stmts)])
       (syntax/loc ctxt
         (#%let
           ([temp expr] ...)
           (#%let
             ([var temp] ...)
             stmt ...
             (#%void))))))]

  [((MethodDef ctxt names attr params block))
   (compile-statement
    (Assignment ctxt
                (list (Subscript ctxt (names->subscripts ctxt names) (symbol->bytes attr)))
                (list (Func ctxt (cons 'self params) block))))]

  [((Protect ctxt value-stmts post-stmts))
   (with-syntax ([(value-stmt ...) (map compile-statement value-stmts)]
                 [(post-stmt ...) (map compile-statement post-stmts)])
     (syntax/loc ctxt
       (#%dynamic-wind
         #%void
         (#%lambda () value-stmt ... (#%void))
         (#%lambda () post-stmt ... (#%void)))))]

  [((Repeat ctxt cond-expr block))
   (with-syntax ([cond-expr (compile-expr cond-expr)]
                 [block (compile-block block)])
     (syntax/loc ctxt
       (#%let/ec #%break (#%let #%repeat () block (#%unless cond-expr (#%repeat))))))]

  [((Return ctxt (list exprs ... (vararg vararg-expr))))
   (with-syntax ([(expr ...) (map compile-expr* exprs)]
                 [vararg-expr (compile-expr vararg-expr)])
     (syntax/loc ctxt
       (#%apply #%return expr ... (#%adjust-va vararg-expr))))]

  [((Return ctxt exprs))
   (with-syntax ([(expr ...) (map compile-expr* exprs)])
     (syntax/loc ctxt
       (#%return expr ...)))]

  [((While ctxt cond-expr block))
   (with-syntax ([cond-expr (compile-expr cond-expr)]
                 [block (compile-block block)])
     (syntax/loc ctxt
       (#%let/ec #%break
         (#%let #%while ()
           (#%when cond-expr
             block
             (#%while))))))])

(define/match (compile-expr* e)
  [((or (Call ctxt _ _)
        (CallMethod ctxt _ _ _)))
   (with-syntax ([expr (compile-expr e)])
     (syntax/loc ctxt
       (#%adjust expr)))]

  [(_)
   (compile-expr e)])

(define/match (compile-expr e)
  [((? boolean?)) (datum->syntax #f e)]
  [((? number?))  (datum->syntax #f e)]
  [((? bytes?))   (datum->syntax #f e)]
  [((? symbol?))  (datum->syntax #f e)]

  [((Attribute ctxt expr name))
   (with-syntax ([expr (compile-expr* expr)]
                 [name (symbol->bytes name)])
     (syntax/loc ctxt
       (#%subscript expr name)))]

  [((Binop ctxt op lhs-expr rhs-expr))
   (with-syntax ([binop op]
                 [lhs-expr (compile-expr* lhs-expr)]
                 [rhs-expr (compile-expr* rhs-expr)])
     (syntax/loc ctxt
       (binop lhs-expr rhs-expr)))]

  [((or (? Call?) (? CallMethod?)))
   (compile-call e)]

  [((Func ctxt (list params ... '...) block))
   (with-syntax ([(param ...) params]
                 [block (compile-block block)])
     (syntax/loc ctxt
       (#%lambda ([param nil] ... . #%rest)
         (#%let/ec #%return block))))]

  [((Func ctxt params block))
   (with-syntax ([(param ...) params]
                 [block (compile-block block)])
     (syntax/loc ctxt
       (#%lambda ([param nil] ... . #%unused-rest)
         (#%let/ec #%return block))))]

  [((Subscript ctxt expr field-expr))
   (with-syntax ([expr (compile-expr* expr)]
                 [field-expr (compile-expr* field-expr)])
     (syntax/loc ctxt
       (#%subscript expr field-expr)))]

  [((Table ctxt (list field-exprs ... (Field _ (vararg vararg-expr)))))
   (with-syntax ([(field-expr ...) (map compile-field field-exprs)]
                 [vararg-expr (compile-expr vararg-expr)])
     (syntax/loc ctxt
       (#%apply #%table field-expr ... (#%adjust-va vararg-expr))))]

  [((Table ctxt field-exprs))
   (with-syntax ([(field-expr ...) (map compile-field field-exprs)])
     (syntax/loc ctxt
       (#%table field-expr ...)))]

  [((Unop ctxt op expr))
   (with-syntax ([unop op]
                 [expr (compile-expr* expr)])
     (syntax/loc ctxt
       (unop expr)))])

(define/match (compile-call _e)
  [((CallMethod ctxt target-expr attr arg-exprs))
   (define subscript-expr (Subscript ctxt '#%instance (symbol->bytes attr)))
   (with-syntax ([target-expr (compile-expr* target-expr)]
                 [call-expr (compile-expr* (Call ctxt subscript-expr (cons '#%instance arg-exprs)))])
     (syntax/loc ctxt
       (#%let ([#%instance target-expr]) call-expr)))]

  [((Call ctxt rator-expr (list rand-exprs ... (vararg vararg-expr))))
   (with-syntax ([rator-expr (compile-expr* rator-expr)]
                 [(rand-expr ...) (map compile-expr* rand-exprs)]
                 [vararg-expr (compile-expr vararg-expr)])
     (syntax/loc ctxt
       (#%apply rator-expr rand-expr ... (#%adjust-va vararg-expr))))]

  [((Call ctxt rator-expr rand-exprs))
   (with-syntax ([rator-expr (compile-expr* rator-expr)]
                 [(rand-expr ...) (map compile-expr* rand-exprs)])
     (syntax/loc ctxt
       (rator-expr rand-expr ...)))])

(define/match (compile-field _e)
  [((Field ctxt expr))
   (with-syntax ([expr (compile-expr* expr)])
     (syntax/loc ctxt
       expr))]

  [((FieldExpr ctxt field-expr value-expr))
   (with-syntax ([field-expr (compile-expr* field-expr)]
                 [value-expr (compile-expr* value-expr)])
     (syntax/loc ctxt
       (#%cons field-expr value-expr)))]

  [((FieldLit ctxt name expr))
   (with-syntax ([name (symbol->bytes name)]
                 [expr (compile-expr* expr)])
     (syntax/loc ctxt
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
      [(cons (LocalAssignment ctxt names exprs) stmts)
       (define node (Let ctxt names exprs (Lua->L1 stmts)))
       (reverse (cons node res))]
      [(cons (LocalFunction ctxt name params block) stmts)
       (define func (Func ctxt params block))
       (define node (Let ctxt (list name) (list func) (Lua->L1 stmts)))
       (reverse (cons node res))]
      [(cons stmt stmts)
       (loop (cons stmt res) stmts)])))


;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-match-expander vararg
  (lambda (stx)
    (syntax-parse stx
      [(_ e) #'(and (or (? Call?) '#%rest) e)])))

(define symbol->bytes
  (compose1 string->bytes/utf-8 symbol->string))

(define (format-label-id name-id)
  (format-id #f "#%label:~a" name-id))

(define (names->subscripts ctxt names)
  (let loop ([target (car names)]
             [names (cdr names)])
    (cond
      [(null? names) target]
      [else
       (define sub (Subscript ctxt target (symbol->bytes (car names))))
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
     [(Attribute ctxt _ name)
      (define temp (format-sym "#%attr-temp~a" idx))
      (Attribute ctxt temp name)]
     [(Subscript ctxt _ _)
      (define lhs-temp (format-sym "#%sub-lhs-temp~a" idx))
      (define rhs-temp (format-sym "#%sub-rhs-temp~a" idx))
      (Subscript ctxt lhs-temp rhs-temp)]
     [_
      var])))

(define (format-sym fmt . args)
  (string->symbol (apply format fmt args)))
