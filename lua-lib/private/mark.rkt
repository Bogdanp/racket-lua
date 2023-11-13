#lang racket/base

(require racket/port)

(provide
 ~call-stack
 function-mark)

(define (~call-stack [indent 3])
  (define indent-str
    (make-string indent #\space))
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
                 (fprintf out "~n~a  [repeated ~a times]" indent-str n))
               (fprintf out "~n~a~a: ~a"
                        indent-str
                        (srcloc->string* (car pair))
                        (cdr pair))
               (values pair 1)]))))))

(define function-mark
  (make-continuation-mark-key))


;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (srcloc->string* loc)
  (srcloc->string
   (struct-copy
    srcloc loc
    [source (and
             (srcloc-source loc)
             (~truncated-source (srcloc-source loc)))])))

(define (~truncated-source source)
  (define source-str
    (if (path? source)
        (path->string source)
        (format "~a" source)))
  (if (> (string-length source-str) 20)
      (format "...~a" (substring source-str (- (string-length source-str) 17)))
      source-str))
