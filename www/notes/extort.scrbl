#lang scribble/manual

@(require (for-label (except-in racket ... compile) a86/printer a86/ast))
@(require redex/pict
          racket/runtime-path
          scribble/examples
          "../fancyverb.rkt"
	  "utils.rkt"
	  "ev.rkt"
	  extort/types
	  extort/semantics
	  "../utils.rkt")



@(define codeblock-include (make-codeblock-include #'h))

@(ev '(require rackunit a86 extort extort/compile-ops extort/correct))

@;{Hack to get un-provided functions from compile-ops}
@(ev '(require (only-in rackunit require/expose)))
@(ev '(require/expose extort/compile-ops [assert-integer assert-char assert-byte assert-codepoint]))


@(define this-lang "Extort")

@title[#:tag this-lang]{@|this-lang|: when errors exist}

@src-code[this-lang]

@emph{The greatest mistake is to imagine that we never err.}

@table-of-contents[]

@section[#:tag "errors"]{Errors}

We have added multiple, disjoint types, but mostly swept issues of
errors under the rug by considering type mismatches as meaningless.
But undefined behavior, while convenient from the compiler writer's
perspective, is the gateway to all kinds of bad outcomes for
programmers.  Let's redesign the semantics to specify the error
behavior of such programs.


We'll call it @bold{@this-lang}.

Nothing changes in the syntax of @this-lang from the previous
language, although we will need to talk about two kinds of
@emph{results} from evaluating programs: values and errors.  We will
say that evaluation produces an @bold{answer}, which is either a value
or error:

@centered{@render-language[E]}

@section{Meaning of @this-lang programs}

Languages adopt several approaches to type mismatches:

@itemlist[

@item{Prohibit such programs statically with a type system (e.g. OCaml, Java)}
@item{Coerce values to different types (e.g. JavaScript)}
@item{Signal a run-time error (e.g. Racket)}
@item{Leave the behavior unspecified (e.g. Scheme, C)}

]

We've previously seen the last approach.  Now let's do what Racket
does and signal an error.


The meaning of @this-lang programs that have type errors will now be
defined as @racket['err].  (You shouldn't make too much out of how we
choose to represent the ``this program caused an error'' answer; all
that really matters at this point is that it is disjoint from values.
Since the langauge doesn't yet include symbols, using a symbol is a
fine choice.):

@itemlist[

@item{@racket[(add1 #f)]: means @racket['err].}

@item{@racket[(zero? #t)]: means @racket['err].}

@item{@racket[(if (zero? #f) 1 2)]: means @racket['err].}

]

@;{
@(define ((rewrite s) lws)
   (define lhs (list-ref lws 2))
   (define rhs (list-ref lws 3))
   (list "" lhs (string-append " " (symbol->string s) " ") rhs ""))

@(require (only-in racket add-between))
@(define-syntax-rule (show-judgment name i j)
   (with-unquote-rewriter
      (lambda (lw)
        (build-lw (lw-e lw) (lw-line lw) (lw-line-span lw) (lw-column lw) (lw-column-span lw)))
      (with-compound-rewriters (['+ (rewrite '+)]
                                ['- (rewrite '–)]
                                ['= (rewrite '=)]
				['!= (rewrite '≠)])
        (apply centered
	   (add-between
             (build-list (- j i)
	                 (λ (n) (begin (judgment-form-cases (list (+ n i)))
	                               (render-judgment-form name))))
             (hspace 4))))))

There are three ways in which an error can be introduced:
@(show-judgment 𝑬 0 3)

And there are four rules for propagating errors from subexpressions:
@(show-judgment 𝑬 3 7)



Now what does the semantics say about @racket[(add1 #f)]?  What about
@racket[(if 7 #t -2)]?
}


In order to define the semantics, we first introduce the type of
results that may be given by the interpretation function:

@#reader scribble/comment-reader
(ex
;; type Answer = Value | 'err
)

Type mismatches can arise as the result of primitive operations being
applied to arguments for which the primitive is undefined, so we
revise @racket[interp-prim1] to check all necessary preconditions
before carrying out an operation.

We will take advantage of Racket's exception system to @racket[raise]
an error result whenever an error occurs.  This is a nice way of
shortcircuiting the remaining evaluation since as soon as we encounter
an error, we know this is going to be the answer for the whole
program.

@codeblock-include["extort/interp-prim.rkt"]

Notice that the signature for these functions indicate that you get a
value, or they raise an exception.  The functions for interpreting
primitives check all of the necessary preconditions of a primitive
before calling the corresponding Racket function.  This ensures that
the Racket functions are always @emph{safe}, they cannot raise an
error.  As a final catch-all case, we know that some precondition must
not have held and we can raise the @racket['err] answer to indicate an
error.

Within the interpreter, we restructure things so that there is now a
top-level @racket[interp] function that installs an exception handler.
Should interpreting the expression raise @racket['err], it handles
this exception and returns the @racket['err] answer.  The
@racket[interp] function makes use of an auxilary function
@racket[interp-e] that does the work of recursively interpreting the
meaning of an expression:

@codeblock-include["extort/interp.rkt"]

We can confirm the interpreter computes the right result for the
examples given earlier:

@ex[
(interp (parse '(add1 #f)))
(interp (parse '(zero? #t)))
(interp (parse '(if (zero? #f) 1 2)))
]

Note that if you use the @racket[interp-e] function, expressions that
cause errors will raise an exception:

@ex[
(eval:error (interp-e (parse '(add1 #f))))
(eval:error (interp-e (parse '(zero? #t))))
(eval:error (interp-e (parse '(if (zero? #f) 1 2))))
]


This interpreter implicitly relies on the state of the input and
output port, but we can define a pure interpreter like before,
which we take as the specification of our language:

@codeblock-include["extort/interp-io.rkt"]

An important property of this semantics is that it provides a meaning
for all possible expressions; in other words, @racket[interp/io] is a
total function, and our language has no undefined behaviors.

@bold{Total Semantics}: @emph{For all @racket[e] @math{∈} @tt{Expr},
there exists @racket[a] @math{∈} @tt{Answer}, @racket[o] @math{∈}
@tt{String}, such that @racket[(interp/io e i)] equals @racket[(cons
a o)].}

The statement of correctness, which is revised to use the answer type
in place of values, reads:

@bold{Compiler Correctness}: @emph{For all @racket[e] @math{∈}
@tt{Expr}, @racket[i], @racket[o] @math{∈} @tt{String}, and @racket[a]
@math{∈} @tt{Answer}, if @racket[(interp/io e i)] equals @racket[(cons
a o)], then @racket[(exec/io e i)] equals
@racket[(cons a o)].}

By virtue of the semantics being total, we have a complete
specification for the compiler.  There are no longer programs for
which it is free to do arbitrary things; it is always obligated to
produce the same answer as the interpreter.


@section{Checking and signalling errors at run-time}

Suppose we want to compile @racket[(add1 #f)], what needs to happen?
Just as in the interpreter, we need to check the integerness of the
argument's value before doing the addition operation.

This checking needs to be emitted as part of the compilation of the
@racket[add1] primitive.  It no longer suffices to simply do an
@racket[Add] instruction on whatever is in the @racket[rax] register;
the compiled code should first check that the value in @racket[rax]
encodes an integer, and if it doesn't, it should somehow stop the
computation and signal that an error has occurred.

The checking part is fairly easy.  Our encoding of values, first
discussed in @secref["Dupe"], devotes some number of bits within a
value to indicate the type.  Checking whether something is an integer
involves inspecting just those parts of the value.

Suppose we have an arbitrary value in @racket[rax].  If it's an
integer, the least significant bit has to be @racket[0].  We could
mask out just that bit by @racket[And]ing the value with the bit
@racket[#,mask-int] and compare the result to type tag for integers
(i.e. @racket[#,type-int]).  Let's write a little function to play
around with this idea:

@#reader scribble/comment-reader
(ex
;; Produces 0 if v is an integer value
(define (is-int? v) ;; Value -> 0 | 1
  (asm-interp
    (prog (Global 'entry)
          (Label 'entry)
          (Mov 'rax (value->bits v))
          (And 'rax mask-int)
          (Ret))))

(is-int? 0)
(is-int? 1)
(is-int? 2)
(is-int? #t)
(is-int? #f))

Unfortunately, the use of @racket[And] here destroys the value in
@racket[rax] in order to determine if it is an integer.  That's fine
for this predicate, but if we wanted to compute something with the
integer, we'd be toast as soon as checked it's type.

Suppose we wanted to write a function that did @racket[add1] in case
the argument is an integer value, otherwise it produces false.  For
that, we would need to use a temporary register for the type tag
check:

@#reader scribble/comment-reader
(ex
;; Produces (add1 v) if v is an integer value, #f otherwise
(define (plus1 v) ;; Value -> Integer | Boolean
  (run
    (prog (Global 'entry)
          (Label 'entry)
          (Mov 'rax (value->bits v))
	  (Mov 'r9 'rax)	  
          (And 'r9 mask-int)
          (Cmp 'r9 type-int)
          (Jne 'err)
          (Add 'rax (value->bits 1))
          (Ret)
          (Label 'err)
          (Mov 'rax (value->bits #f))
          (Ret))))

(plus1 0)
(plus1 1)
(plus1 2)
(plus1 #t)
(plus1 #f))

This is pretty close to how we can implement primitives like
@racket[add1].  The only missing piece is that instead of returning a
specific @emph{value}, like @racket[#f], we want to stop computation
and signal that an error has occurred.  To accomplish this we add a
function called @tt{raise_error} to our run-time system that, when
called, prints @tt{err} and exits with a non-zero number to signal an
error.  The @racket[asm-interp] intercepts these calls are returns the
@racket['err] symbol to match what the interpreter does:

@ex[
(run
  (prog (Global 'entry)
        (Label 'entry)
        (Extern 'raise_error)
        (Call 'raise_error)))]

Now we can make a function that either does the addition or signals an
error:

@#reader scribble/comment-reader
(ex
;; Produces (add1 v) if v is an integer, 'err otherwise
(define (plus1 v) ;; Value -> Integer | 'err
  (run
    (prog (Global 'entry)
          (Label 'entry)
          (Mov 'rax (value->bits v))
	  (Mov 'r9 'rax)	  
          (And 'r9 mask-int)
          (Cmp 'r9 type-int)
          (Jne 'err)
          (Add 'rax (value->bits 1))
          (Ret)
          (Label 'err)
	  (Extern 'raise_error)
          (Call 'raise_error))))

(plus1 0)
(plus1 1)
(plus1 2)
(plus1 #t)
(plus1 #f))

This can form the basis of how primitives with error checking can be
implemented.  Looking at @racket[interp-prim1], we can see that in
addition to checking for whether an argument is an integer, we will
also need checks for characters, bytes, and unicode codepoints; the
latter two are refinements of the integer check: they require there
arguments to be integers in a certain range.

To support these checks, we develop a small library of type assertion
functions that, given a register name, produce code to check the type
of value held in the register, jumping to @racket[err] whenever the
value is not of the asserted type:

@itemlist[
@item{@racket[assert-integer] @tt{: Register -> Asm} produces code to check that the value in the given register is an integer,}
@item{@racket[assert-char] @tt{: Register -> Asm} produces code to check that the value in the given register is a character,}
@item{@racket[assert-byte] @tt{: Register -> Asm} produces code to check that the value in the given register is an integer and in the range [0,256).}
@item{@racket[assert-codepoint] @tt{: Register -> Asm} produces code to check that the value in the given register is an integer and either in the range [0,55295] or [57344, 1114111].}
]

@codeblock-include["extort/assert.rkt"]

The compiler for primitive operations is updated to include
appropriate type assertions:

@codeblock-include["extort/compile-ops.rkt"]

@ex[
(compile-op1 'add1)
(compile-op1 'char->integer)
(compile-op1 'write-byte)]


The top-level compiler largely stays the same, but it now declares an
external label @racket['raise_error] that will be defined by the
run-time system and defines a label called @racket['err] that calls
@tt{raise_error}:

@codeblock-include["extort/compile.rkt"]

@ex[
(compile-e (parse '(add1 #f)))
(compile-e (parse '(char->integer #\a)))
(compile-e (parse '(write-byte 97)))]



@section{Run-time for @this-lang}


We extend the run-time system with a C function called
@tt{raise_error} that prints "err" and exits with a non-zero status to
indicate something has gone wrong.  @margin-note{The runtime system is
written with a level of indirection between @tt{raise_error} and the
code that prints and exits in @tt{error_exit}; this is done so that
the testing framework can intercede and replace the error function,
but it can be ignored.}

@filebox-include[fancy-c extort "main.c"]


Linking in the run-time allows us to define the @racket[exec] and
@racket[exec/io] functions:

@codeblock-include["extort/exec.rkt"]

We can run examples:

@ex[
(exec (parse '(add1 8)))
(exec (parse '(add1 #f)))]



@section{Correctness, revisited}

This allows to re-formulate the check for compiler correctness check
in its earlier, simpler form that just blindly runs the interpreter
and compiler and checks that the results are the same, thanks to the
totality of the semantics:

@codeblock-include["extort/correct.rkt"]

@ex[
(check-compiler (parse '(add1 8)) "")
(check-compiler (parse '(add1 #f)) "")]

And again, we can randomly test the compiler by generating programs and inputs:

@ex[
(require extort/random)
(for ((i 100))
  (check-compiler (random-expr) (random-input)))]
