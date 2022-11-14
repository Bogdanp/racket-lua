#lang racket/base

(require (for-syntax racket/base)
         racket/runtime-path
         "error.rkt"
         "iter.rkt"
         "json.rkt"
         "length.rkt"
         "nil.rkt"
         "number.rkt"
         "relation.rkt"
         "string.rkt"
         "table.rkt"
         "type.rkt")


;; foreign access ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 current-racket-imports-enabled?)

(define current-racket-imports-enabled?
  (make-parameter #f))

(define-runtime-module-path-index racket/base 'racket/base)
(define-runtime-module-path-index racket/file 'racket/file)
(define-runtime-module-path-index racket/port 'racket/port)

(define (require-racket id-bytes [mod #"racket/base"])
  (unless (current-racket-imports-enabled?)
    (lua:error "require_racket: disabled"))
  (dynamic-require
   (case mod
     [(#"racket/base") racket/base]
     [(#"racket/file") racket/file]
     [(#"racket/port") racket/port]
     [else (string->symbol (bytes->string/utf-8 mod))])
   (string->symbol (bytes->string/utf-8 id-bytes))))

(define lua:racket
  (make-table `(#"#%require" . ,require-racket)))


;; global environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.lua.org/manual/5.4/manual.html#2.2

(provide
 make-initial-environment
 current-global-environment)

(define (make-initial-environment)
  (define arg
    (apply make-table
           (for/list ([arg (in-vector (current-command-line-arguments))])
             (string->bytes/utf-8 arg))))
  (define env
    (make-table
     `(#"_VERSION"     . #"racket-lua 0.1")
     `(#"arg"          . ,arg)
     `(#"assert"       . ,lua:assert)
     `(#"error"        . ,lua:error)
     `(#"getmetatable" . ,lua:getmetatable)
     `(#"ipairs"       . ,lua:ipairs)
     `(#"json"         . ,(make-json-module))
     `(#"next"         . ,lua:next)
     `(#"pairs"        . ,lua:pairs)
     `(#"pcall"        . ,lua:pcall)
     `(#"print"        . ,lua:print)
     `(#"racket"       . ,lua:racket)
     `(#"rawequal"     . ,lua:rawequal)
     `(#"rawget"       . ,lua:rawget)
     `(#"rawlen"       . ,lua:rawlen)
     `(#"rawset"       . ,lua:rawset)
     `(#"setmetatable" . ,lua:setmetatable)
     `(#"tonumber"     . ,lua:tonumber)
     `(#"tostring"     . ,lua:tostring)
     `(#"type"         . ,lua:type)))
  (begin0 env
    (table-set! env #"_G" env)))

(define current-global-environment
  (make-parameter (make-initial-environment)))


;; stdlib ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 current-standard-library-modules
 load-standard-library!)

(define-runtime-module-path-index coroutine.lua "../stdlib/coroutine.lua")
(define-runtime-module-path-index file.lua "../stdlib/file.lua")
(define-runtime-module-path-index io.lua   "../stdlib/io.lua")
(define-runtime-module-path-index math.lua "../stdlib/math.lua")
(define-runtime-module-path-index os.lua "../stdlib/os.lua")
(define-runtime-module-path-index racket.lua "../stdlib/racket.lua")
(define-runtime-module-path-index string.lua "../stdlib/string.lua")
(define-runtime-module-path-index table.lua "../stdlib/table.lua")

(define (load-table.lua! env name)
  (define mod (car (dynamic-require table.lua '#%chunk)))
  (table-set! env name mod)
  (table-set! env #"select" (table-ref mod #"select")))

(define current-standard-library-modules
  (make-parameter
   (list
    `(#"racket"    . ,racket.lua)    ;; everything depends on racket.lua

    `(#"file"      . ,file.lua)
    `(#"os"        . ,os.lua)
    `(#"table"     . ,load-table.lua!)

    `(#"coroutine" . ,coroutine.lua) ;; depends on table.lua
    `(#"io"        . ,io.lua)        ;; depends on file.lua and os.lua
    `(#"math"      . ,math.lua)
    `(#"string"    . ,string.lua))))

(define (load-standard-library! env)
  (for ([p (in-list (current-standard-library-modules))])
    (define name (car p))
    (define path-or-proc (cdr p))
    (parameterize ([current-racket-imports-enabled? #t]
                   [current-standard-library-modules null]
                   [current-global-environment env])
      (cond
        [(procedure? path-or-proc)
         (path-or-proc env name)]
        [else
         (define chunk (dynamic-require path-or-proc '#%chunk))
         (table-set! env name (car chunk))]))))
