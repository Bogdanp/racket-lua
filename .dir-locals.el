(("lua-lib/lang/compiler.rkt" . ((nil . ((eval . (cl-dolist (s '("#%cond" "#%define" "#%dynamic-wind" "#%lambda" "#%let/ec" "#%let" "#%unless" "#%when"))
                                                   (put (intern s) 'racket-indent-function #'defun))))))))
