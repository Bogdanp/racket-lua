#lang racket/base

(provide
 lua:pcall
 lua:error)

(struct exn:fail:lua exn:fail (value level))

(define (lua:pcall proc . args)
  (with-handlers ([exn:fail:lua? (λ (e) (values #f (exn:fail:lua-value e)))]
                  [exn:fail? (λ (e) (values #f (exn-message e)))])
    (values #t (apply proc args))))

(define (lua:error v [level 1] . _)
  (raise (exn:fail:lua "error" (current-continuation-marks) v level)))
