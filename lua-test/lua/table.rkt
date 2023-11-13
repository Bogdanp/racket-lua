#lang racket/base

(require lua/value
         rackcheck
         racket/list
         racket/match
         rackunit)

(define table-tests
  (test-suite
   "table"

   (test-suite
    "in-table"

    (check-equal? (for/list ([v (in-table (make-table))]) v) null)
    (check-equal?
     (for/list ([v (in-table (make-table 1 2 3))]) v)
     '(1 2 3))
    (check-equal?
     (for/list ([v (in-table (make-table nil 1 nil 2 nil nil 3))])
       v)
     (list nil 1 nil 2 nil nil 3)))

   (test-suite
    "table-length"

    (test-case "sequences"
      (check-property
       (property ([n (gen:integer-in 0 1000)])
         (check-equal?
          (table-length
           (apply make-table (make-list n #t)))
          n))))

    ;; Generate random sequences with gaps in them and ensure valid
    ;; borders are returned. Remove a random number of inserted elements
    ;; to avoid always hitting the (not (nil? (table-ref t table-hi)))
    ;; case.
    (test-case "sequences with gaps"
      (check-property
       (property ([groups (gen:integer-in 0 16)]
                  [instrs (apply
                           gen:tuple
                           (for/list ([n (in-range groups)])
                             (define gen:len (gen:integer-in n (add1 (* n 1000))))
                             (gen:frequency
                              `((3 . ,(gen:let ([m gen:len]) `(gap ,m)))
                                (2 . ,(gen:let ([m gen:len]) `(rem ,m)))
                                (7 . ,gen:len)))))])
         (define t (make-table))
         (define last-idx
           (for/fold ([idx 1])
                     ([instr (in-list instrs)])
             (match instr
               [`(gap ,len) (+ idx len)]
               [`(rem ,_) idx]
               [len
                (for ([i (in-range len)])
                  (table-set! t (+ idx i) #t))
                (+ idx len)])))
         (for/fold ([idx last-idx])
                   ([instr (in-list instrs)])
           (match instr
             [`(rem ,len)
              (for ([i (in-range len)])
                (table-set! t (- idx i) nil))
              (- idx len)]
             [_ idx]))
         (define border
           (table-length t))
         (label!
          (if (zero? border)
              "zero"
              "non-zero"))
         (cond
           [(zero? border)
            (check-true
             (nil? (table-ref t 1)))]
           [else
            (check-false
             (nil? (table-ref t border)))
            (check-true
             (nil? (table-ref t (add1 border))))])))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests table-tests))
