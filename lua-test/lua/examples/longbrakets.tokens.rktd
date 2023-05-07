#hasheqv((0 . #s(token op "#" |#| 1 0 1))
         (1 . #s(token name "lang" lang 1 1 2))
         (2 . #s(token whitespace " " " " 1 5 6))
         (3 . #s(token name "lua" lua 1 6 7))
         (4 . #s(token whitespace "\n\n" "\n\n" 1 9 10))
         (5
          .
          #s(token
             comment
             "--[ not a long bracket comment"
             "--[ not a long bracket comment"
             3
             0
             12))
         (6 . #s(token whitespace "\n" "\n" 4 0 43))
         (7
          .
          #s(token
             comment
             "--[[\n    long brackets comments\n    have multiline support\n--]]"
             "--[[\n    long brackets comments\n    have multiline support\n--]]"
             5
             0
             44))
         (8 . #s(token whitespace "\n\n" "\n\n" 8 4 107))
         (9
          .
          #s(token
             comment
             "--[=[\n    [[ nesting ]]\n--]=]"
             "--[=[\n    [[ nesting ]]\n--]=]"
             10
             0
             109))
         (10 . #s(token whitespace "\n\n" "\n\n" 12 5 138))
         (11 . #s(token name "print" print 14 0 140))
         (12 . #s(token lparen "(" "(" 14 5 145))
         (13
          .
          #s(token string "\"after comments\"" #"after comments" 14 6 146))
         (14 . #s(token rparen ")" ")" 14 22 162))
         (15 . #s(token whitespace "\n\n" "\n\n" 14 23 163))
         (16 . #s(token name "print" print 16 0 165))
         (17 . #s(token lparen "(" "(" 16 5 170))
         (18 . #s(token string "[[test]]" #"test" 16 6 171))
         (19 . #s(token rparen ")" ")" 16 14 179))
         (20 . #s(token whitespace "\n" "\n" 16 15 180))
         (21 . #s(token name "print" print 17 0 181))
         (22 . #s(token lparen "(" "(" 17 5 186))
         (23
          .
          #s(token
             string
             "[[\n  multi-line\n  test\n]]"
             #"  multi-line\n  test\n"
             17
             6
             187))
         (24 . #s(token rparen ")" ")" 20 2 212))
         (25 . #s(token whitespace "\n" "\n" 20 3 213))
         (26 . #s(token name "print" print 21 0 214))
         (27 . #s(token lparen "(" "(" 21 5 219))
         (28
          .
          #s(token
             string
             "[==[\n  [[nesting]]\n]==]"
             #"  [[nesting]]\n"
             21
             6
             220))
         (29 . #s(token rparen ")" ")" 23 4 243))
         (30 . #s(token whitespace "\n" "\n" 23 5 244)))
