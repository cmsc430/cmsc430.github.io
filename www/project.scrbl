#lang scribble/manual
@(require (for-label (except-in racket compile ...) a86))
@(require "defns.rkt")
@(require "utils.rkt")
@(require "notes/ev.rkt")
@(require "notes/utils.rkt")
@(require "fancyverb.rkt")
@(require (for-label (except-in racket ...)))
@(require racket/runtime-path)

@title[#:tag "Project" #:style '(unnumbered)]{Project}

@(ev '(require loot-exceptions))


The final assessment for this course consists of an individually
completed project.

@bold{Due: @final-date, @final-end-time}


You have two options for the final project.  Select whichever one
you'd prefer:

@itemlist[
@item{ Add exception handling to Loot }
@item{ Add integer arithmetic to Loot }
]


@section[#:style 'unnumbered]{Exception Handling}


For this project, you are tasked with adding an exception
handling mechanism to Loot.

An exception handler consists of a predicate and a function.  When a
value is raised to a handler, the predicate is applied and if it
produces a true value, the raised value is ``handled'' by applying the
function to it.  If the predicate returns @racket[#f], the value is
raised to the next exception handler.  An error is signalled if a
raised value is never handled by an exception handler.


The key features that need to be added:

@itemlist[

@item{@racket[(raise _e)] will evaluate @racket[_e] and then ``raise''
the value to the most recently installed exception handler, thereby
abandoning whatever computation surrounds the @racket[(raise _e)]
expression up to the next exception handler.  If there is no exception
handler installed, an error is signalled.}

@item{@racket[(with-handlers ([_e₁ _e₂]) _e)] will evaluate
@racket[_e₁], then @racket[_e₂].  The value of @racket[_e₁] and
@racket[_e₂] are then installed as the current exception handler
predicate and function, respectively, while @racket[_e] is evaluated.

Should @racket[_e] produce a value @racket[_v], then the exception
handler is removed and the @racket[with-handlers] expression evaluates
to @racket[_v].  But if in evaluating @racket[_e], a value is
@emph{raised} that reaches this exception handler, the predicate is
applied.  If the predicate returns a true value, the result of the
@racket[with-handlers] expression is computed by applying the handler
function to the raised value.  If the predicate returns false, the
value continues being raised, thereby abandoning whatever computation
surrounds the @racket[with-handlers] expression up to the next
exception handler.}

]

You are given a zip file @tt{loot-exceptions.zip} on ELMS that is like
Loot, but the parser, AST, and interpreter have been extended to
include these new features.  You have to extend the compiler to match
the specification of the interpreter.

Some examples:


@ex[
(interp (parse '(raise 1)))
]

This signals an error because there is no exception handler installed
that handles the raised value @racket[1].

@ex[
(interp
  (parse
    '(with-handlers ([(λ (x) #t) (λ (x) 0)])
       1)))
]

This evaluates to @racket[1] because even though an exception handler
is installed, the expression in the body of the @racket[with-handlers]
expression never raises.


@ex[
(interp
 (parse
  '(with-handlers ([(λ (x) #t) (λ (x) 0)])
     (raise 1))))
]

This evaluates to @racket[0] because an exception handler has been
installed before the value @racket[1] is raised; the predicate of the
handler returns @racket[#t] when applied to @racket[1], so the result
of the @racket[with-handlers] expression is computed by applying the
function @racket[(λ (x) 0)] to @racket[1], which evaluates to
@racket[0].

@ex[
(interp
 (parse
   '(with-handlers ([(λ (x) #f) (λ (x) 0)])
      (raise 1))))
]

This signals an error because although there is an exception handler
installed when @racket[1] is raised, the predicate returns
@racket[#f], so value continues to be raised to the exception handler
installed before the @racket[with-handlers] expression was evaluated,
but there is no such handler.

@ex[
(interp
 (parse
   '(with-handlers ([(λ (x) #t) (λ (x) 2)])
      (with-handlers ([(λ (x) #f) (λ (x) 0)])
        (raise 1)))))
]

This evaluates to @racket[2] because @racket[1] is raised to the most
recent handler, which does not handle it (the predicate returns
@racket[#f]), but then @racket[1] is raised to the next handler, which
does handle it (the predicate returns @racket[#t]), and thus the
function @racket[(λ (x) 2)] is applied to the raised value @racket[1] to compute
the result, @racket[2].

Note that when a value is raised, it abandons the computation at the
point of the raise.

@ex[
(interp
 (parse
   '(with-handlers ([(λ (x) #t) (λ (x) x)])
      (+ (raise 1) (add1 #f)))))]

Here the addition computation is abandoned when @racket[1] is raised.
Consequently, the expression @racket[(add1 #f)] is never evaluated
and the whole expression evaluates to @racket[1].

Since the handler predicate and function are arbitrary functions, they
two can raise exceptions, which will be handled by the the surrounding
exception handlers.

@ex[
(interp
 (parse
   '(with-handlers ([(λ (x) (if (zero? x) #t (raise 0))) (λ (x) x)])
      (raise 1))))]
      
This signals an error because in applying the predicate to @racket[1],
the value @racket[0] is raised, and there is no enclosing handler.

It's important to note that the idea of the most recently installed
handler is a dynamic property; it is not a lexical property.  A raise
expression may occur lexically @emph{outside} of the
@racket[with-handler] form that ends up handling it.

Consider:

@ex[
(interp
  (parse
    '(define (f x) (raise x))
    '(with-handlers ([(lambda (x) #t) (lambda (x) x)])
       (f 3))))]

Here, calling @racket[f] triggers the raising of @racket[3], which is
handled by the @racket[with-handlers] expression, producing @racket[3].

@subsection[#:style 'unnumbered]{Differences with Racket's exception system}

There are important differences between this project's specification
and Racket's exception system.

In our approach, errors are distinct from exceptions.  For example
@racket[(add1 #f)] signals an error.  It does not raise an exception
and therefore it cannot be handled with an exception handler:

@ex[
(interp
  (parse
    '(with-handlers ([(λ (x) #t) (λ (x) 1)])
       (add1 #f))))]

Racket on the other hand, uses its exception mechanism to signal errors:

@ex[
(with-handlers ([(λ (x) #t) (λ (x) 1)])
  (add1 #f))
]

Consequently Racket evaluates this expression to @racket[1] for this example.

This simplifies things in our setting because the only way that
something can be raised is an explicit @racket[raise] expression and
anything that used to be an error continues to be an error.

Since there are subtle differences between the semantics of Loot
exceptions and Racket exceptions, we recommend always using the
interpreter for guidance on what a particular example should do.

Another difference is syntactic: Racket allows each
@racket[with-handlers] form to have any number of predicate and
function clauses, but for our purposes, we assume each
@racket[with-handlers] has exactly one predicate and function.

@subsection[#:style 'unnumbered]{Hints on implementation}

An implementation of exception handling for Loot can be fairly
succinct, but tricky.

The basic idea is that a handler is installed by
@racket[with-handlers] by pushing some information on the stack before
executing the code for the body expression.  This information will
include the predicate value and function value for the handler.


If execution of the body makes it through to the end, then a value is
being returned normally, and we can pop off the handler information
from the stack.  However, if at some point during the execution of the
body, there's a raise, then we need to start the process of handling
the raised value, applying the predicate, etc.  A basic problem though
is that there may be an arbitrary amount of stuff pushed on to the
stack between the handler information and the point when the raise
occurs.

For example:

@ex[
(define (f x)
   (if (zero? x)
       (raise x)
       (+ x (f (sub1 x)))))

(with-handlers ([(λ (x) #t) (λ (x) 1)])
  (f 100))]

The handler is pushed on the stack.  As the recursion unfolds, return
pointers, arguments, and intermediate results will be pushed on the
stack.  This continues until reaching the base case, at which point
the raise occurs.  We now need the handler, but how?  We can't
possibly use the compile-time environment to compute its location on
the stack because it lives past that portion of the stack.  But it is
also worth noting that everything on the stack up to the handler
should be discarded because the raise discards the computation that
has built up to this point: all those pending function calls will
never be returned to.  Instead we need to transfer control back to the
@racket[with-handlers] expression and execute the code that follows
it.  That means jumping back to the @racket[with-handlers], but how
will @racket[raise] know where to jump?  We can store a return pointer
together with the predicate and function.  That way the raise can use
the return address to jump back.  But the issue remains: how does
raise find all this handler information on stack?

A simple approach is to designate a register to hold a pointer to the
handler on the stack.  When executing a @racket[raise], the stack can
be popped back to the handler, giving us access to the predicate, the
function, and the return label used to jump back to the
@racket[with-handlers] expression.

This works great when there's exactly one exception handler, but what
happens when handlers are nested?  It would seem a single register to
point into the stack won't suffice.  The fix is to add a fourth piece
of information to the handler information: the pointer to the parent
handler.

So to summarize: each handler is represented by 4 things pushed on
the stack:
@itemlist[

@item{a predicate,}

@item{a function,}

@item{a return label,}

@item{and a pointer to the parent handler on the stack (if there is
one).}]

And a designated register holds a pointer to the current handler.


@section[#:style 'unnumbered]{Integer Arithmetic}

For this project, you are tasked with extending the set of integer
values to include all integers, not just those that fit within a
register.

Since the existing Loot interpreter already handles arbitrary
integers, there is no new syntax or semantics for this feature.
Instead, you will need to resolve the long-standing bug that has
been in our compilers since Abscond.


The key features that need to be added:

@itemlist[

@item{arbitrary integer literals}

@item{extension of all of the arithmetic operations to work with arbitrary integers}]

You are given a zip file @tt{loot-bignums.zip} on ELMS that is like
Loot.  In fact, it is exactly the Loot implementation we studied.  The
only change is the addition of a new tagged pointer data type that can
be used for representing integers outside the range of immediate
integers (those that fit in a register with the int type tag).

@subsection[#:style 'unnumbered]{Hint on implementation}

An implementation of arbitrarily large integers involves designing a
good representation for these new kinds of values and then overloading
arithmetic operations to work on both immediate integers (called
fixnums) and pointer-based integers (called bignums), and in the case
of binary operations, combinations of these kinds of integers.

Here's a sketch of how bignums could be represented (but you are free
to design the representation however you'd like).


A bignum can be a tagged pointer to a bit indicating the sign of the
number and a linked list of ``digits'' in order of increasing
significance.  What we mean by ``digit'' is up to our choosing, but
basically it's an integer in some fixed range that can fit into a
register.  To explain the idea, let's take this range to be [0,9]
i.e. literally digits.  The same approach will work for larger bases
and for the real implementation, you'll want to pick a larger base.

Let's forget about the sign for a bit and assume we're only dealing
with positive bignums and let's talk about the algorithm at a
high-level.

With this representation in mind, numbers outside the range [0,9] can
be represented by sequences of digits.  For example, @racket[192]
would be represented as @tt{2 9 1}; @racket[2025] would be @tt{5 2 0
2}.  Notice that the least significant digit is first and the most
significant is last.

Doing addition of two numbers follows the grade-school algorithm of
adding each place of the numbers, from least to most significant
digit, possibly with a carry.  Whenever we reach the end of one of the
numbers, we take an remaining digits from the other number.

For example adding @tt{2 9 1} to @tt{5 2 0 2}, would go digit by digit:

@itemlist[

@item{add @racket[2] to @racket[5] to get @racket[7],}

@item{add @racket[9] to @racket[2] to get @racket[1], carrying @racket[1],}

@item{add @racket[1] to @racket[0] and the carried @racket[1] to get @racket[2],}

@item{and now take the remaining digits, in this case just @racket[2].}]

Thus the sum is @tt{7 1 2 2}, aka @racket[2217], which is the sum of @racket[2025] and @racket[192].

Notice that each step of the computation only involves adding digits
together and the sum of two digits can be represented as a single
digit and possibly a carry.  Also notice that this approach works no
matter what base we choose.

Suppose we used base 100, then @racket[192] would be represented as
@tt{92 1} and @racket[2025] would be represented as @tt{25 20}.

To add them together:

@itemlist[

@item{add @racket[92] to @racket[25] to get @racket[17], carry @racket[1]}
@item{add @racket[1] to @racket[20] and the carried @racket[1] to get @racket[22]}]

Thus we get @tt{17 22}, aka @racket[2217].


Now the idea for bignums is to use a large base for the ``digits,'' so
long as it's not larger than @math{2^64}.  The sum and possible carry
of each pair of digits can be carried out with arithmetic instructions
and machine integers and the arbitrarily large sequence of ``digits''
is represented using memory.

The core ideas and algorithms here are pretty simple but the details
will involve some careful thought.  @bold{You probably will save
yourself a lot of trouble by working the details out using a
high-level language first before trying to implement everything in
assembly.}

@subsection[#:style 'unnumbered]{Bits to values}

Because this project involves your own memory representation for
bignums, you will need to add functionality in @tt{types.rkt} to
@racket[bits->value] so that Racket integers can be reconstructed from
your representation of bignums.  This is needed to test any programs
that return bignum values.  But this shouldn't stop you from writing
tests using expressions that compute intermediate results that are
bignums, but whose final result is not a bignum.  Several such tests
are provided in the starter code.

You do not need to implement printing for bignums in the C run-time
system.  We will not test that code at all.

@section[#:style 'unnumbered]{Submitting}

Submit a zip file containing your work to Gradescope to the Final
Project assignment.  Use @tt{make submit.zip} to create a zip file
with the proper structure.  The autograder will determine which
project you did based on the code you submit.


