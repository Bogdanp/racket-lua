#lang scribble/manual

@(require (for-label lua/env
                     lua/value
                     racket/base
                     racket/contract))

@title{Lua}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]

@(define (lua-anchor text)
   (link "https://lua.org" text))

This package provides a @hash-lang[] implementation of the
@lua-anchor{Lua programming language}.

@section{Calling Lua from Racket}

Lua modules can be imported directly from Racket.  Every Lua module
provides a single value called @racket[#%chunk] which represents the
return value of that module.  For example, if you save the following
program to a file named "add1.lua":

@verbatim{
#lang lua

function add1(x)
  return 1 + x
end

return {add1 = add1}
}

You can use it from Racket like so:

@racketblock[
(require lua/value "add1.lua")
(define add1 (table-ref #%chunk #"add1"))
(add1 5)
]

@section{Differences from Lua}
@subsection{@tt{goto}}

@subsection{Integers}

Integers are backed by regular Racket @racket[integer?] values, so
the cannot overflow.

@section{Reference}
@subsection{Values}
@defmodule[lua/value]

@defthing[lua-value/c (or/c boolean? bytes? number? nil? procedure? table?)]{
  The contract that identifies Lua values.
}

@deftogether[(
  @defthing[nil lua-value/c]
  @defproc[(nil? [v any/c]) boolean?]
)]{
  The nil value and the predicate that identifies it.
}

@defproc[(table? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a table.
}

@defproc[(make-table [v (or/c lua-value/c (cons/c lua-value/c lua-value/c))] ...) table?]{
  Creates a @racket[table?].
}

@defproc[(table-ref [t table?]
                    [k lua-value/c]
                    [default-proc (-> lua-value/c) #f]) lua-value/c]{
  Retrieves the value at @racket[k] from @racket[t].  If @racket[t]
  doesn't contain @racket[k], the @racket[default-proc] is called.
  When not provided, the @racket[default-proc] looks up missing keys
  in the table's metatable.
}

@defproc[(table-set! [t table?]
                     [k lua-value/c]
                     [v lua-value/c]) void?]{
  Adds @racket[v] to @racket[t] under @racket[v], replacing any
  existing values.  If @racket[v] is @racket[nil], the key is removed
  from the table instead.
}

@defproc[(table-length [t table?]) exact-nonnegative-integer?]{
  Returns a border of @racket[t].
}

@defproc[(lua:getmetatable [t table?]) (or/c nil? table?)]{
  Returns @racket[t]'s metatable.
}

@defproc[(lua:setmetatable [t table?] [meta table?]) table?]{
  Sets @racket[t]'s metatable to @racket[meta] and returns @racket[t].
}

@subsection{Environments}
@defmodule[lua/env]

@defparam[current-global-environment env table? #:value (make-initial-environment)]{
  Holds the global environment that is used when Lua chunks are
  evaluated.
}

@defproc[(make-initial-environment) table?]{
  Returns a @racket[table?] representing the initial environment.
}
