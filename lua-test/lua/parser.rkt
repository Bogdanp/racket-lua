#lang racket/base

(require (for-syntax racket/base
                     racket/syntax-srcloc
                     syntax/parse/pre)
         lua/lang/ast
         lua/lang/lexer
         lua/lang/parser
         rackunit)

(define (parse str)
  (->sexp (parse-chunk (make-lexer (open-input-string str)))))

(define (check-parse* loc str expected)
  (with-check-info*
    (list (make-check-location loc))
    (λ () (check-equal? (parse str) expected))))

(define-syntax (check-parse stx)
  (syntax-parse stx
    [(_ str:expr expected:expr)
     (define check-location
       (let ([loc (syntax-srcloc #'expected)])
         (list
          (srcloc-source loc)
          (srcloc-line loc)
          (srcloc-column loc)
          (srcloc-position loc)
          (srcloc-span loc))))
     #`(check-parse* #,(cons 'list check-location) str 'expected)]))

(define parser-tests
  (test-suite
   "parser"

   (test-suite
    "statements"
    (check-parse
     ";;;print(1);;print(a)"
     (Block
      ((Call (Name print) (1))
       (Call (Name print) ((Name a)))))))

   (test-suite
    "assignment"

    (check-parse "a = 1" (Block ((Assignment ((Name a)) (1)))))
    (check-parse "a, b = 1" (Block ((Assignment ((Name a) (Name b)) (1)))))
    (check-parse "a = 1, 'b'" (Block ((Assignment ((Name a)) (1 #"b")))))
    (check-parse
     "foo()['a'] = 1"
     (Block
      ((Assignment
        ((Subscript (Call (Name foo) ()) #"a"))
        (1)))))
    (check-parse
     "foo()['a'], bar:baz()['c'] = 2, a:b():c()"
     (Block
      ((Assignment
        ((Subscript (Call (Name foo) ()) #"a")
         (Subscript (CallMethod (Name bar) (Name baz) ()) #"c"))
        (2
         (CallMethod (CallMethod (Name a) (Name b) ()) (Name c) ())))))))

   (test-suite
    "function call"

    (check-parse
     "print('hello')"
     (Block ((Call (Name print) (#"hello")))))
    (check-parse
     "Account.withdraw(a, 1)"
     (Block ((Call (Attribute (Name Account) (Name withdraw)) ((Name a) 1)))))
    (check-parse
     "a:withdraw(1)"
     (Block ((CallMethod (Name a) (Name withdraw) (1))))))

   (test-suite
    "expression"

    (test-suite
     "unary ops"

     (check-parse "print(not true)" (Block ((Call (Name print) ((Unop (Name not) #t))))))
     (check-parse "print(#table)" (Block ((Call (Name print) ((Unop (Name #%length) (Name table)))))))
     (check-parse "print(~42)" (Block ((Call (Name print) ((Unop (Name #%bnegate) 42))))))
     (check-parse
      "print(not true and false)"
      (Block
       ((Call (Name print) ((Binop (Name and) (Unop (Name not) #t) #f))))))
     (check-parse
      "print(not (true and false))"
      (Block
       ((Call (Name print) ((Unop (Name not) (Binop (Name and) #t #f)))))))
     (check-parse
      "print(#table > 5)"
      (Block
       ((Call (Name print) ((Binop (Name >) (Unop (Name #%length) (Name table)) 5))))))
     (check-parse
      "print(~x^2)"
      (Block
       ((Call (Name print) ((Unop (Name #%bnegate) (Binop (Name ^) (Name x) 2)))))))
     (check-parse
      "print(~x^2 + 3)"
      (Block
       ((Call (Name print) ((Binop (Name +) (Unop (Name #%bnegate) (Binop (Name ^) (Name x) 2)) 3)))))))

    (test-suite
     "binary ops"

     (check-parse
      "print(1 + 2 * (3 - 4 // 2 ^ 2) == 12 and false or true)"
      (Block
       ((Call
         (Name print)
         ((Binop
           (Name or)
           (Binop
            (Name and)
            (Binop
             (Name ==)
             (Binop
              (Name +)
              1
              (Binop
               (Name *)
               2
               (Binop
                (Name -)
                3
                (Binop
                 (Name //)
                 4
                 (Binop (Name ^) 2 2)))))
             12)
            #f)
           #t))))))


     (check-parse
      "print(1 + 2 * (3 - 4 // 2 ^ 2) == 12 and not false or true)"
      (Block
       ((Call
         (Name print)
         ((Binop
           (Name or)
           (Binop
            (Name and)
            (Binop
             (Name ==)
             (Binop
              (Name +)
              1
              (Binop
               (Name *)
               2
               (Binop
                (Name -)
                3
                (Binop
                 (Name //)
                 4
                 (Binop (Name ^) 2 2)))))
             12)
            (Unop (Name not) #f))
           #t))))))

     (check-parse
      "print(1 + 2 ^ 3 ^ 4)"
      (Block
       ((Call
         (Name print)
         ((Binop
           (Name +)
           1
           (Binop
            (Name ^)
            2
            (Binop
             (Name ^)
             3
             4))))))))

     (check-parse
      "print('a' .. 'b' .. 'c')"
      (Block
       ((Call
         (Name print)
         ((Binop
           (Name ..)
           #"a"
           (Binop
            (Name ..)
            #"b"
            #"c")))))))

     (check-parse
      "print(('a' .. 'b') .. 'c')"
      (Block
       ((Call
         (Name print)
         ((Binop
           (Name ..)
           (Binop
            (Name ..)
            #"a"
            #"b")
           #"c")))))))

    (test-suite
     "function"

     (check-parse
      #<<EOF
print(function()
  return 42
end)
EOF
      (Block
       ((Call
         (Name print)
         ((Func () (Block ((Return (42))))))))))

     (check-parse
      #<<EOF
(function(x)
  return x + x
end)(42)
EOF
      (Block
       ((Call
         (Func ((Name x)) (Block ((Return ((Binop (Name +) (Name x) (Name x)))))))
         (42)))))

     (check-parse
      #<<EOF
(function(x)
  return x + x, 42
end)(42)
EOF
      (Block
       ((Call
         (Func ((Name x)) (Block ((Return ((Binop (Name +) (Name x) (Name x)) 42)))))
         (42))))))
    (test-suite
     "table"

     (check-parse "print({})" (Block ((Call (Name print) ((Table ()))))))
     (check-parse "print({1, 2, 3})" (Block ((Call (Name print) ((Table ((Field 1) (Field 2) (Field 3))))))))

     (check-parse
      "t = {1, [0 + 1] = 1, foo = 2}"
      (Block
       ((Assignment
         ((Name t))
         ((Table
           ((Field 1)
            (FieldExpr (Binop (Name +) 0 1) 1)
            (FieldLit (Name foo) 2))))))))))))

(module+ test
  (require rackunit/text-ui)
  (parameterize ([port-count-lines-enabled #t])
    (run-tests parser-tests)))
