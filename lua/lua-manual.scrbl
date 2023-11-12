#lang scribble/manual

@(require scribble/example
          (for-label lua/env
                     lua/value
                     racket/base
                     racket/contract
                     racket/sandbox))

@title{Lua}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]
@defmodule[lua]

@(define (lua-anchor text)
   (link "https://lua.org" text))

This package provides a @hash-lang[] implementation of the
@lua-anchor{Lua programming language}.  It is still a work in
progress, but much of the core language is supported already.


@section{Calling Lua from Racket}
@subsection{Requiring Modules}

Lua modules can be imported directly from Racket.  Every Lua module
provides a list called @racket[#%chunk] which represents the set of
return values of that module.  For example, if you save the following
program to a file named "add1.lua":

@codeblock|{
#lang lua

function add1(x)
  return 1 + x
end

return {add1 = add1}
}|

You can use it from Racket like so:

@racketblock[
(require lua/value "add1.lua")
(define add1 (table-ref (car #%chunk) #"add1"))
(add1 5)
]

@subsection{Evaluating Code at Runtime}

You can run Lua code at runtime using @racketmodname[racket/sandbox].
For example:

@(define ev (make-base-eval '(require racket/string)))

@examples[
#:label #f
#:eval ev
  (require racket/sandbox)
  ((make-module-evaluator "#lang lua\nreturn 42") '#%chunk)
  ((make-module-evaluator "#lang lua\nreturn 42, 43") '#%chunk)

  (code:line)
  (define dangerous-prog
    (string-append
     "#lang lua\n"
     "\n"
     "return io.input('/etc/passwd'):read('a')"))

  (eval:error
   ((make-module-evaluator dangerous-prog) '#%chunk))

  (define chunk
    (parameterize ([sandbox-path-permissions '((read #rx#".*"))])
      ((make-module-evaluator dangerous-prog) '#%chunk)))
  (subbytes (car chunk) 0 5)
]

@section{Calling Racket from Lua}

Lua modules can access values from @racketmodname[racket/base] by
indexing into the @tt{racket} global.  This is only allowed when the
value of @racket[current-racket-imports-enabled?] is @racket[#t].

@codeblock|{
#lang lua

local list = racket.list
print(list(1, 2, 3))
}|

You can also alter the global environment in any way you like via the
@racket[current-global-environment] parameter.


@section{Differences from Lua}
@subsection{Chunks}

Chunks may return multiple values, but everything after the first
value is discarded when the results get bound to @tt{#%chunk}.

@subsection{The @tt{goto} Statement and Labels}

Labels are lexically scoped so the @tt{goto} statement cannot jump to
labels that have not yet been defined at the time the statement is
executed or that are outside its scope.

@subsection[#:tag "diffs-values"]{Values}

Integers are backed by regular Racket @racket[integer?] values, so
they do not overflow.


@section{REPL}

You can run a Lua REPL by loading the @tt{lua/repl} module:

@verbatim{  racket -l lua/repl -i  }

Note that unlike a typical REPL, since lua is statement oriented, you
must @tt{return} values for them to be printed.  Multi-line statements
can be entered by bracketing them with @tt[":{"] and @tt[":}"].  For
example:

@verbatim|{
> :{
| do
|   local x = 42
|   print(x)
| end
| :}
42
}|

Individual @tt{local} declarations at the top level have no effect.


@section{Reference}
@subsection{Values}
@defmodule[lua/value]

@defthing[lua-value/c (or/c boolean? bytes? number? nil? procedure? table?)]{
  The contract that identifies Lua values.  Note that Lua strings map
  to @racket[bytes?] values.
}

@deftogether[(
  @defthing[nil nil?]
  @defproc[(nil? [v any/c]) boolean?]
)]{
  The nil value and the predicate that identifies it.
}

@defproc[(table? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a table.
}

@defproc[(make-table [v (or/c lua-value/c (cons/c lua-value/c lua-value/c))] ...) table?]{
  Creates a @racket[table?].  Plain values are indexed sequentially,
  @racket[cons]ed values represent key-value pairs and do not modify
  the sequence number.

  @examples[
    (require lua/value)
    (make-table 1 2 nil 3 (cons #"a" #"b"))
  ]
}

@defproc[(in-table [t table?]) sequence?]{
  Converts @racket[t] to a sequence, returning values up to one of its
  borders.
}

@defproc[(table-ref [t table?]
                    [k lua-value/c]
                    [default-proc (-> lua-value/c) _default]) lua-value/c]{
  Retrieves the value at @racket[k] from @racket[t].  If @racket[t]
  doesn't contain @racket[k], the @racket[default-proc] is called.
  When not provided, the @racket[default-proc] looks up missing keys
  in the table's metatable.

  This procedure is used during index lookups in Lua code.
}

@defproc[(table-set! [t table?]
                     [k lua-value/c]
                     [v lua-value/c]) void?]{
  Adds @racket[v] to @racket[t] under @racket[v], replacing any
  existing values.  If @racket[v] is @racket[nil], the key is removed
  from the table instead.

  This procedure is used during index assignments in Lua code.
}

@defproc[(table-length [t table?]) exact-nonnegative-integer?]{
  Returns a border of @racket[t].
}

@defproc[(table-metatable [t table?]) (or/c nil? table?)]{
  Returns @racket[t]'s metatable.

  This procedure is accessible from Lua code as @tt{getmetatable}.
}

@defproc[(set-table-metatable! [t table?] [meta table?]) table?]{
  Sets @racket[t]'s metatable to @racket[meta] and returns @racket[t].

  This procedure is accessible from Lua code as @tt{setmetatable}.
}

@subsection{Environments}
@defmodule[lua/env]

@defproc[(make-initial-environment) table?]{
  Returns a @racket[table?] representing the initial environment.
  This procedure creates a new table each time it is called, but the
  values inside it may be shared between calls.
}

@defparam[current-racket-imports-enabled? enabled? boolean? #:value #f]{
  Controls whether or not Lua modules can access Racket code via the
  @tt{racket} global.  Defaults to @racket[#f], except during the time
  when the Lua standard library (distributed with this package) is
  loaded.
}

@defparam[current-global-environment env table? #:value (make-initial-environment)]{
  Holds the global environment that is used when Lua chunks are
  evaluated.
}

@defparam[current-standard-library-modules modules
                                           (listof (cons/c bytes?
                                                           (or/c module-path?
                                                                 resolved-module-path?
                                                                 module-path-index?)))]{
  Holds the set of modules that are loaded into every Lua module.  You
  can remove values from this set in order to disable certain bits of
  functionality (eg. file i/o), or you may add new modules to the set
  to inject your own functionality.  The modules are loaded in order,
  and there may be dependencies from latter modules to earlier ones.

  Modules in this list are typically only instantiated once so they
  must not modify the global environment because the global
  environment may change to a fresh table after these modules are
  loaded for the first time.
}
