#lang scribble/manual

@(require (for-label lua/env
                     lua/value
                     racket/base
                     racket/contract))

@title{Lua}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]
@defmodule[lua]

@(define (lua-anchor text)
   (link "https://lua.org" text))

This package provides a @hash-lang[] implementation of the
@lua-anchor{Lua programming language}.  It is still a work in
progress, but much of the core language is supported already.


@section{Calling Lua from Racket}

Lua modules can be imported directly from Racket.  Every Lua module
provides a single value called @racket[#%chunk] which represents the
return value of that module.  For example, if you save the following
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
(define add1 (table-ref #%chunk #"add1"))
(add1 5)
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
@subsection{@tt{goto}}

The @tt{goto} keyword may only be used to jump to labels defined
within the dynamic extent of the @tt{goto} statement itself.  That is,
you may not jump to labels ``in the future.''

@subsection{Integers}

Integers are backed by regular Racket @racket[integer?] values, so
they cannot overflow.

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

@defproc[(make-initial-environment) table?]{
  Returns a @racket[table?] representing the initial environment.
}
