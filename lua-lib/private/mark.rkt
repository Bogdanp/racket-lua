#lang racket/base

(require racket/port)

(provide
 ~call-stack
 function-mark)

(define (~call-stack)
  (define stack
    (reverse
     (continuation-mark-set->list
      (current-continuation-marks)
      function-mark)))
  (and (not (null? stack))
       (call-with-output-string
        (lambda (out)
          (for/fold ([prev #f] [n 0])
                    ([pair (in-list stack)])
            (cond
              [(equal? pair prev)
               (values pair (add1 n))]
              [else
               (when (> n 1)
                 (fprintf out "~n  [repeats ~a times]" n))
               (fprintf out "~n~a:~a"
                        (srcloc->string (car pair))
                        (cdr pair))
               (values pair 1)]))))))

(define function-mark
  (make-continuation-mark-key))
