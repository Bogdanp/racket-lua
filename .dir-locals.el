(("lua-lib/lang/compiler.rkt" . ((nil . ((eval . (cl-dolist (s '("#%cond" "#%define" "#%lambda" "#%let/ec" "#%let" "#%when"))
                                                   (put (intern s) 'racket-indent-function #'defun))))))))
