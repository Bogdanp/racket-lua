(("lua-lib/lang/compiler.rkt" . ((nil . ((eval . (cl-dolist (s '("#%cond" "#%define" "#%dynamic-wind" "#%lambda" "#%let/cc" "#%let" "#%unless" "#%when"))
                                                   (put (intern s) 'racket-indent-function #'defun))))))))
