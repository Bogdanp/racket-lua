#lang racket/base

(require (for-syntax racket/base
                     racket/syntax-srcloc
                     syntax/parse)
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
      ((Call print (1))
       (Call print (a))))))

   (test-suite
    "assignment"

    (check-parse "a = 1" (Block ((Assignment (a) (1)))))
    (check-parse "a, b = 1" (Block ((Assignment (a b) (1)))))
    (check-parse "a = 1, 'b'" (Block ((Assignment (a) (1 #"b")))))
    (check-parse
     "foo()['a'] = 1"
     (Block
      ((Assignment
        ((Subscript (Call foo ()) #"a"))
        (1)))))
    (check-parse
     "foo()['a'], bar:baz()['c'] = 2, a:b():c()"
     (Block
      ((Assignment
        ((Subscript (Call foo ()) #"a")
         (Subscript (CallMethod bar baz ()) #"c"))
        (2
         (CallMethod (CallMethod a b ()) c ())))))))

   (test-suite
    "function call"

    (check-parse
     "print('hello')"
     (Block ((Call print (#"hello")))))
    (check-parse
     "Account.withdraw(a, 1)"
     (Block ((Call (Attribute Account withdraw) (a 1)))))
    (check-parse
     "a:withdraw(1)"
     (Block ((CallMethod a withdraw (1))))))

   (test-suite
    "expression"

    (test-suite
     "unary ops"

     (check-parse "print(not true)" (Block ((Call print ((Call not (#t)))))))
     (check-parse "print(#table)" (Block ((Call print ((Call #%length (table)))))))
     (check-parse "print(~42)" (Block ((Call print ((Call #%bitwise-not (42)))))))
     (check-parse
      "print(not true and false)"
      (Block
       ((Call print ((Call and ((Call not (#t)) #f)))))))
     (check-parse
      "print(not (true and false))"
      (Block
       ((Call print ((Call not ((Call and (#t #f)))))))))
     (check-parse
      "print(#table > 5)"
      (Block
       ((Call print ((Call > ((Call #%length (table)) 5)))))))
     (check-parse
      "print(~x^2)"
      (Block
       ((Call print ((Call #%bitwise-not ((Call ^ (x 2)))))))))
     (check-parse
      "print(~x^2 + 3)"
      (Block
       ((Call print ((Call + ((Call #%bitwise-not ((Call ^ (x 2)))) 3))))))))

    (test-suite
     "binary ops"

     (check-parse
      "print(1 + 2 * (3 - 4 // 2 ^ 2) == 12 and false or true)"
      (Block
       ((Call print ((Call or ((Call and ((Call == ((Call + (1 (Call * (2 (Call - (3 (Call // (4 (Call ^ (2 2)))))))))) 12))
                                          #f))
                               #t)))))))

     (check-parse
      "print(1 + 2 * (3 - 4 // 2 ^ 2) == 12 and not false or true)"
      (Block
       ((Call print ((Call or ((Call and ((Call == ((Call + (1 (Call * (2 (Call - (3 (Call // (4 (Call ^ (2 2)))))))))) 12))
                                          (Call not (#f))))
                               #t)))))))

     (check-parse
      "print(1 + 2 ^ 3 ^ 4)"
      (Block
       ((Call print ((Call + (1 (Call ^ (2 (Call ^ (3 4)))))))))))

     (check-parse
      "print('a' .. 'b' .. 'c')"
      (Block
       ((Call print ((Call .. (#"a" (Call .. (#"b" #"c")))))))))

     (check-parse
      "print(('a' .. 'b') .. 'c')"
      (Block
       ((Call print ((Call .. ((Call .. (#"a" #"b")) #"c"))))))))

    (test-suite
     "function"

     (check-parse
      #<<EOF
print(function()
  return 42
end)
EOF
      (Block
       ((Call print ((Func () (BlockWithReturn () (42))))))))

     (check-parse
      #<<EOF
(function(x)
  return x + x
end)(42)
EOF
      (Block
       ((Call
         (Func (x) (BlockWithReturn () ((Call + (x x)))))
         (42)))))

     (check-parse
      #<<EOF
(function(x)
  return x + x, 42
end)(42)
EOF
      (Block
       ((Call
         (Func (x) (BlockWithReturn () ((Call + (x x)) 42)))
         (42))))))

    (test-suite
     "table"

     (check-parse "print({})" (Block ((Call print ((Table ()))))))
     (check-parse "print({1, 2, 3})" (Block ((Call print ((Table ((Field 1) (Field 2) (Field 3))))))))

     (check-parse
      "t = {1, [0 + 1] = 1, foo = 2}"
      (Block
       ((Assignment
         (t)
         ((Table
           ((Field 1)
            (FieldExpr (Call + (0 1)) 1)
            (FieldLit foo 2))))))))))))

(module+ test
  (require rackunit/text-ui)
  (parameterize ([port-count-lines-enabled #t])
    (run-tests parser-tests)))
