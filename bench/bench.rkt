#lang racket/base

(require json
         racket/match
         racket/port
         racket/runtime-path
         racket/sandbox
         racket/system)

(define-runtime-path programs-dir "programs")

(struct config (name path args)
  #:transparent)

(define programs
  (list
   (config "fannkuch"
           (build-path programs-dir "fannkuch.lua")
           '("10"))
   (config "spectral-norm"
           (build-path programs-dir "spectral-norm.lua")
           '("1000"))
   (config "binary-trees"
           (build-path programs-dir "binary-trees.lua")
           '("15"))))

(define-runtime-path results.json
  "results.json")

(define-runtime-path HEAD
  "../.git/refs/heads/master")

(define (get-git-commit)
  (call-with-input-file HEAD port->string))

(define lua
  (find-executable-path "lua"))

(define (run-lua . args)
  (match-define (list stdout _stdin _pid stderr control)
    (apply process* lua args))
  (control 'wait)
  (define code
    (control 'exit-code))
  (unless (zero? code)
    (error 'run-lua "lua process failed: ~a" (port->string stderr)))
  (port->string stdout))

(define (get-lua-version)
  (run-lua "-v"))

(define (eval-lua lua-in args)
  (define-values (in out)
    (make-pipe))
  (thread
   (lambda ()
     (displayln "#lang lua" out)
     (copy-port lua-in out)
     (close-output-port out)))
  (call-with-trusted-sandbox-configuration
   (lambda ()
     (parameterize ([sandbox-input (current-input-port)]
                    [sandbox-output (current-output-port)]
                    [sandbox-error-output (current-error-port)]
                    [sandbox-init-hook (λ () (current-command-line-arguments (list->vector args)))])
       ((make-module-evaluator in) '#%chunk)))))

(define (bench proc)
  (define-values (_ cpu real gc)
    (time-apply proc null))
  (hasheq 'cpu cpu 'real real 'gc gc))

(module+ main
  (require racket/cmdline)

  (define programs-to-run
    (command-line
     #:args program-names
     (if (null? program-names)
         programs
         (for/list ([conf (in-list programs)]
                    #:when (member (config-name conf) program-names))
           conf))))

  (define data
    (with-handlers ([exn:fail? (λ (_) null)])
      (call-with-input-file results.json read-json)))
  (define lua-version (get-lua-version))

  (define results
    (for/hash ([conf (in-list programs-to-run)])
      (match-define (config name path args) conf)
      (printf "== Running ~a (lua) ==~n" name)
      (define lua-result
        (hash-set*
         (bench
          (lambda ()
            (display (apply run-lua (path->string path) args))))
         'version lua-version))
      (printf "== Running ~a (racket) ==~n" name)
      (define racket-result
        (bench
         (lambda ()
           (call-with-input-file path
             (lambda (in)
               (eval-lua in args))))))
      (values
       (string->symbol name)
       (hasheq
        'lua lua-result
        'racket racket-result))))

  (define new-data
    (cons
     (hasheq
      'timestamp (current-seconds)
      'commit (get-git-commit)
      'machine (system-type 'machine)
      'results results)
     data))
  (call-with-output-file results.json
    #:exists 'replace
    (lambda (out)
      (write-json new-data out))))