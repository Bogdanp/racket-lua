#lang racket/base

(require racket/runtime-path)

(define-runtime-path results.json
  "results.json")

(module+ main
  (require json
           plot
           racket/cmdline)

  (define commit-re #rx".*")
  (define machine-re #rx".*")
  (command-line
   #:once-each
   ["--commit-re" COMMIT-RE
                  "the regular expression to use on commit messages when selecting results"
                  (set! commit-re (regexp COMMIT-RE))]
   ["--machine-re" MACHINE-RE
                  "the regular expression to use on machine messages when selecting results"
                  (set! machine-re (regexp MACHINE-RE))])

  (define data
    (findf
     (λ (e)
       (and (regexp-match? commit-re (hash-ref e 'commit))
            (regexp-match? machine-re (hash-ref e 'machine))))
     (with-handlers ([exn:fail? (λ (_)
                                  (eprintf "error: failed to read results.json~n")
                                  (exit 1))])
       (call-with-input-file results.json read-json))))
  (unless data
    (eprintf "error: no matching results~n")
    (exit 1))
  (define results (hash-ref data 'results))

  (define (make-histogram key x-min)
    (discrete-histogram
     #:label (string-titlecase (symbol->string key))
     #:x-min x-min #:skip 2.5
     #:color (add1 x-min) #:line-color (add1 x-min)
     (for*/list ([program (in-list (sort (hash-keys results) symbol<?))]
                 [program-res (in-value (hash-ref results program))])
       (list
        (string-titlecase (symbol->string program))
        (hash-ref (hash-ref program-res key) 'real)))))

  (parameterize ([plot-new-window? #t]
                 [plot-width 800]
                 [plot-height 400]
                 [plot-title (format "Commit: ~a" (hash-ref data 'commit))]
                 [plot-x-label "Program"]
                 [plot-y-label "Wall Clock Time (ms)"])
    (plot
     (for/list ([(k idx) (in-indexed (in-list '(lua racket)))])
       (make-histogram k idx)))))
