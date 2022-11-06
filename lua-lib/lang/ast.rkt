#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(provide
 (struct-out Node))

(struct Node (ctxt) #:transparent)

(define-syntax (define-node-types stx)
  (syntax-parse stx
    [(_ [Id:id (fld:id ...)] ...)
     #:with (Id? ...)
     (for/list ([stx (in-list (syntax-e #'(Id ...)))])
       (format-id stx "~a?" stx))
     #:with ((Id-fld ...) ...)
     (for/list ([id-stx (in-list (syntax-e #'(Id ...)))]
                [flds-stx (in-list (syntax-e #'((fld ...) ...)))])
       (for/list ([fld-stx (in-list (syntax-e flds-stx))])
         (format-id fld-stx "~a-~a" id-stx fld-stx)))
     #'(begin
         (struct Id Node (fld ...) #:transparent) ...
         (provide (struct-out Id) ...)

         (provide ->sexp)
         (define (->sexp n)
           (cond
             [(Id? n)
              `(Id ,(->sexp (Id-fld n)) ...)] ...
             [(list? n)
              (map ->sexp n)]
             [else
              n])))]))

(define-node-types
  [Assignment (vars exprs)]
  [Attribute (e name)]
  [Block (stmts)]
  [Break ()]
  [Call (expr args)]
  [CallMethod (expr name args)]
  [Do (block)]
  [Field (expr)]
  [FieldExpr (expr value)]
  [FieldLit (name value)]
  [For (name initial-expr limit-expr step-expr block)]
  [Func (params block)]
  [FuncDef (name params block)]
  [Goto (label)]
  [If (cond-expr then-block else-block)]
  [Label (name)]
  [LocalAssignment (name expr)]
  [Repeat (cond-expr block)]
  [Return (exprs)]
  [Subscript (e sub-e)]
  [Table (fields)]
  [While (cond-expr block)]

  [Let (name stmts)])
