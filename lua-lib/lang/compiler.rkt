#lang racket/base

(require racket/match
         racket/syntax
         "ast.rkt")

(provide
 compile-chunk)

(define (compile-chunk chunk)
  (with-syntax ([block (compile-block chunk)])
    #'((#%provide #%chunk)
       (#%define _ENV (#%global))
       (#%define (#%chunk-proc . #%rest) (#%let/ec #%return block))
       (#%define #%chunk (#%chunk-proc)))))

(define/match (compile-block _)
  [((Block ctxt stmts))
   (with-syntax ([(statement ...) (map compile-statement (Lua->L1 stmts))])
     (syntax/loc ctxt
       (#%begin statement ... (#%void))))])

(define/match (compile-statement _)
  [((Assignment ctxt vars '(#%rest)))
   (with-syntax ([((var idx) ...) (for/list ([idx (in-naturals 1)]
                                             [var (in-list vars)])
                                    (list (compile-expr var) idx))])
     (syntax/loc ctxt
       (#%let
        ([#%t (#%table #%rest)])
        (#%set! var (#%table-ref #%t idx)) ...)))]

  [((Assignment ctxt vars exprs))
   (with-syntax ([((var expr) ...) (for/list ([var (in-list vars)]
                                              [expr (in-list exprs)])
                                     (list (compile-expr var)
                                           (compile-expr expr)))])
     (syntax/loc ctxt
       (#%begin (#%set! var expr) ...)))]

  [((Break ctxt))
   (syntax/loc ctxt
     (#%break))]

  [((Call ctxt expr args))
   (with-syntax ([expr (compile-expr expr)]
                 [(arg-expr ...) (map compile-expr args)])
     (syntax/loc ctxt
       (expr arg-expr ...)))]

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
        (#%when (== #%step 0) (#%error "for" "zero step"))
        (#%let #%for ([name #%init])
               (#%when
                (#%cond [(< #%step 0) (>= name #%limit)]
                        [(> #%step 0) (<= name #%limit)])
                block
                (#%for (+ name #%step)))))))]

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
       (#%define (name [param nil] ...) (#%let/ec #%return block))))]

  ;; FIXME: goto needs to be able to jump forwards.
  [((Goto ctxt name))
   (with-syntax ([name (format-label-id name)])
     (syntax/loc ctxt
       (name name)))]

  [((If ctxt cond-expr then-block #f))
   (with-syntax ([cond-expr (compile-expr cond-expr)]
                 [then-block (compile-block then-block)])
     (syntax/loc ctxt
       (#%when cond-expr then-block)))]

  [((If ctxt cond-expr then-block (? If? elseif-block)))
   (with-syntax ([cond-expr (compile-expr cond-expr)]
                 [then-block (compile-block then-block)]
                 [else-block (compile-statement elseif-block)])
     (syntax/loc ctxt
       (#%cond
        [cond-expr
         then-block
         (#%void)]
        [#%else
         else-block
         (#%void)])))]

  [((If ctxt cond-expr then-block else-block))
   (with-syntax ([cond-expr (compile-expr cond-expr)]
                 [then-block (compile-block then-block)]
                 [else-block (compile-block else-block)])
     (syntax/loc ctxt
       (#%cond
        [cond-expr
         then-block
         (#%void)]
        [#%else
         else-block
         (#%void)])))]

  [((Label ctxt name))
   (with-syntax ([name (format-label-id name)])
     (syntax/loc ctxt
       (#%define name (#%call/cc #%values))))]

  [((Let ctxt names stmts))
   (with-syntax ([(name ...) names]
                 [(stmt ...) (map compile-statement stmts)])
     (syntax/loc ctxt
       (#%let ([name nil] ...) stmt ...)))]

  [((Repeat ctxt cond-expr block))
   (with-syntax ([cond-expr (compile-expr cond-expr)]
                 [block (compile-block block)])
     (syntax/loc ctxt
       (#%let/ec #%break (#%let #%repeat () block (#%unless cond-expr (#%repeat))))))]

  [((Return ctxt exprs))
   (with-syntax ([(expr ...) (map compile-expr exprs)])
     (syntax/loc ctxt
       (#%return expr ...)))]

  [((While ctxt cond-expr block))
   (with-syntax ([cond-expr (compile-expr cond-expr)]
                 [block (compile-block block)])
     (syntax/loc ctxt
       (#%let/ec #%break (#%let #%while () (#%when cond-expr block (#%while))))))])

(define/match (compile-expr e)
  [('nil)         (datum->syntax #f e)]
  [((? boolean?)) (datum->syntax #f e)]
  [((? number?))  (datum->syntax #f e)]
  [((? bytes?))   (datum->syntax #f e)]
  [((? symbol?))  (datum->syntax #f e)]

  [((Call ctxt e args))
   (with-syntax ([rator (compile-expr e)]
                 [(rand ...) (map compile-expr args)])
     (syntax/loc ctxt
       (rator rand ...)))]

  [((Func ctxt (list params ... '...) block))
   (with-syntax ([(param ...) params]
                 [block (compile-block block)])
     (syntax/loc ctxt
       (#%lambda ([param nil] ... . #%rest) (#%let/ec #%return block))))]

  [((Func ctxt params block))
   (with-syntax ([(param ...) params]
                 [block (compile-block block)])
     (syntax/loc ctxt
       (#%lambda ([param nil] ...) (#%let/ec #%return block))))]

  [((Subscript ctxt expr field-expr))
   (with-syntax ([expr (compile-expr expr)]
                 [field-expr (compile-expr field-expr)])
     (syntax/loc ctxt
       (#%subscript expr field-expr)))]

  [((Table ctxt fields))
   (with-syntax ([(field ...) (map compile-field fields)])
     (syntax/loc ctxt
       (#%table field ...)))])

(define/match (compile-field e)
  [((Field ctxt expr))
   (with-syntax ([expr (compile-expr expr)])
     (syntax/loc ctxt
       expr))]

  [((FieldExpr ctxt field-expr value-expr))
   (with-syntax ([field-expr (compile-expr field-expr)]
                 [value-expr (compile-expr value-expr)])
     (syntax/loc ctxt
       (#%cons field-expr value-expr)))]

  [((FieldLit ctxt name expr))
   (with-syntax ([name name]
                 [expr (compile-expr expr)])
     (syntax/loc ctxt
       (#%cons name expr)))])


;; passes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lua: The standard lua language.
;; L1: Removes LocalAssignment, adds Let

(define (Lua->L1 stmts)
  (let loop ([res null]
             [stmts stmts])
    (match stmts
      [(list)
       (reverse res)]
      [(cons (LocalAssignment ctxt names exprs) stmts)
       (define assignment (Assignment ctxt names exprs))
       (define node (Let ctxt names (cons assignment (Lua->L1 stmts))))
       (reverse (cons node res))]
      [(cons stmt stmts)
       (loop (cons stmt res) stmts)])))


;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (format-label-id name-id)
  (format-id #f "#%label:~a" name-id))
