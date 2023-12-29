#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse/pre))

(provide
 (struct-out Node))

(struct Node (loc) #:transparent)

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
         (provide ->sexp (struct-out Id) ...)
         (define (->sexp n)
           (cond
             [(Id? n) `(Id ,(->sexp (Id-fld n)) ...)] ...
             [(list? n) (map ->sexp n)]
             [else n])))]))

(define-node-types
  [Assignment (vars exprs)]
  [Attribute (e name)]
  [Binop (name lhs-expr rhs-expr)]
  [Block (stmts)]
  [Break ()]
  [Call (expr args)]
  [CallMethod (expr name args)]
  [Do (block)]
  [Field (expr)]
  [FieldExpr (expr value)]
  [FieldLit (name value)]
  [For (name initial-expr limit-expr step-expr block)]
  [ForIn (names exprs block)]
  [Func (params block)]
  [FuncDef (name params block)]
  [Goto (label)]
  [If (cond-expr then-block else-block)]
  [Label (name)]
  [LocalAssignment (name expr)]
  [LocalFunction (name params block)]
  [MethodDef (name attr params block)]
  [Name (symbol)]
  [Repeat (cond-expr block)]
  [Return (exprs)]
  [Subscript (e sub-e)]
  [Table (fields)]
  [Unop (name expr)]
  [While (cond-expr block)]

  ;; L1
  [Let (vars exprs stmts)]
  [LetFunction (name params block stmts)]
  [Protect (value-stmts post-stmts)])
