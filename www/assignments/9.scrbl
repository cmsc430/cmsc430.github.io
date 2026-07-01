#lang scribble/manual
@(require "../defns.rkt")
@(require "../notes/ev.rkt"
          "../notes/utils.rkt")
@(require (for-label (except-in racket ...)))

@title[#:tag "Assignment 9" #:style 'unnumbered]{Assignment 9: Exceptions}

@bold{Due: @assign-deadline[9]}

@bold{Starter code:} @link["code/loot-exceptions.zip"]{@tt{loot-exceptions.zip}}

@(ev '(require loot-exceptions))

The goal of this assignment is to add an exception handling mechanism
to the Loot compiler.

An exception handler consists of a predicate and a function.  When a
value is raised to a handler, the predicate is applied and if it
produces a true value, the raised value is handled by applying the
function to it.  If the predicate returns @racket[#f], the value is
raised to the next exception handler.  An error is signalled if a
raised value is never handled by an exception handler.

Use the starter code in
@link["code/loot-exceptions.zip"]{@tt{loot-exceptions.zip}}, which is
like Loot, but the parser, AST, and interpreter have been extended to
include these new features.  You have to extend the compiler to match
the specification of the interpreter.

The key features that need to be added are:

@itemlist[

@item{@racket[(raise _e)] evaluates @racket[_e] and then raises the
value to the most recently installed exception handler, thereby
abandoning whatever computation surrounds the @racket[(raise _e)]
expression up to the next exception handler.  If there is no exception
handler installed, an error is signalled.}

@item{@racket[(with-handlers ([_e1 _e2]) _e)] evaluates
@racket[_e1], then @racket[_e2].  The values of @racket[_e1] and
@racket[_e2] are installed as the current exception handler predicate
and function, respectively, while @racket[_e] is evaluated.

Should @racket[_e] produce a value @racket[_v], the exception handler
is removed and the @racket[with-handlers] expression evaluates to
@racket[_v].  But if in evaluating @racket[_e], a value is raised that
reaches this exception handler, the predicate is applied.  If the
predicate returns a true value, the result of the
@racket[with-handlers] expression is computed by applying the handler
function to the raised value.  If the predicate returns false, the
value continues being raised, thereby abandoning whatever computation
surrounds the @racket[with-handlers] expression up to the next
exception handler.}

]

@section[#:tag-prefix "a9-" #:style 'unnumbered]{Examples}

@ex[
(interp (parse '(raise 1)))
]

This signals an error because there is no exception handler installed
that handles the raised value @racket[1].

@ex[
(interp
  (parse
    '(with-handlers ([(lambda (x) #t) (lambda (x) 0)])
       1)))
]

This evaluates to @racket[1] because even though an exception handler
is installed, the body of the @racket[with-handlers] expression never
raises.

@ex[
(interp
 (parse
  '(with-handlers ([(lambda (x) #t) (lambda (x) 0)])
     (raise 1))))
]

This evaluates to @racket[0] because the predicate returns
@racket[#t] when applied to @racket[1], so the result of the
@racket[with-handlers] expression is computed by applying the handler
function to @racket[1].

@ex[
(interp
 (parse
   '(with-handlers ([(lambda (x) #f) (lambda (x) 0)])
      (raise 1))))
]

This signals an error because although there is an exception handler
installed when @racket[1] is raised, the predicate returns
@racket[#f], so the value continues to be raised to the enclosing
exception handler, but there is no such handler.

@ex[
(interp
 (parse
   '(with-handlers ([(lambda (x) #t) (lambda (x) 2)])
      (with-handlers ([(lambda (x) #f) (lambda (x) 0)])
        (raise 1)))))
]

This evaluates to @racket[2] because @racket[1] is raised to the most
recent handler, which does not handle it, and then to the next handler,
which does handle it.

Note that when a value is raised, it abandons the computation at the
point of the raise.

@ex[
(interp
 (parse
   '(with-handlers ([(lambda (x) #t) (lambda (x) x)])
      (+ (raise 1) (add1 #f)))))
]

Here the addition computation is abandoned when @racket[1] is raised.
Consequently, the expression @racket[(add1 #f)] is never evaluated and
the whole expression evaluates to @racket[1].

Since the handler predicate and function are arbitrary functions, they
too can raise exceptions, which will be handled by the surrounding
exception handlers.

@ex[
(interp
 (parse
   '(with-handlers ([(lambda (x) (if (zero? x) #t (raise 0)))
                     (lambda (x) x)])
      (raise 1))))
]

This signals an error because in applying the predicate to
@racket[1], the value @racket[0] is raised, and there is no enclosing
handler.

It's important to note that the most recently installed handler is a
dynamic property; it is not a lexical property.  A @racket[raise]
expression may occur lexically outside of the @racket[with-handlers]
form that ends up handling it.

@ex[
(interp
  (parse
    '(define (f x) (raise x))
    '(with-handlers ([(lambda (x) #t) (lambda (x) x)])
       (f 3))))
]

Here, calling @racket[f] triggers the raising of @racket[3], which is
handled by the @racket[with-handlers] expression, producing
@racket[3].

@section[#:tag-prefix "a9-" #:style 'unnumbered]{Differences with Racket's exception system}

There are important differences between Loot exceptions and Racket's
exception system.

In Loot, errors are distinct from exceptions.  For example,
@racket[(add1 #f)] signals an error.  It does not raise an exception
and therefore it cannot be handled with an exception handler:

@ex[
(interp
  (parse
    '(with-handlers ([(lambda (x) #t) (lambda (x) 1)])
       (add1 #f))))
]

Racket, on the other hand, uses its exception mechanism to signal
errors:

@ex[
(with-handlers ([(lambda (x) #t) (lambda (x) 1)])
  (add1 #f))
]

Consequently, Racket evaluates this expression to @racket[1] for this
example.

This simplifies things in Loot because the only way something can be
raised is an explicit @racket[raise] expression and anything that used
to be an error continues to be an error.

Since there are subtle differences between Loot exceptions and Racket
exceptions, use the interpreter for guidance on what a particular
example should do.

Another difference is syntactic: Racket allows each
@racket[with-handlers] form to have any number of predicate and
function clauses, but for our purposes, each @racket[with-handlers]
has exactly one predicate and function.

@section[#:tag-prefix "a9-" #:style 'unnumbered]{Implementation hints}

An implementation of exception handling for Loot can be fairly
succinct, but tricky.

The basic idea is that a handler is installed by
@racket[with-handlers] by pushing some information on the stack before
executing the code for the body expression.  This information will
include the predicate value and function value for the handler.

If execution of the body makes it through to the end, a value is being
returned normally, and the handler information can be popped off the
stack.  However, if at some point during execution of the body there is
a raise, then the compiler needs to start the process of handling the
raised value, applying the predicate, and so on.  A basic problem is
that there may be an arbitrary amount of stuff pushed on to the stack
between the handler information and the point where the raise occurs.

For example:

@ex[
(define (f x)
   (if (zero? x)
       (raise x)
       (+ x (f (sub1 x)))))

(with-handlers ([(lambda (x) #t) (lambda (x) 1)])
  (f 100))
]

The handler is pushed on the stack.  As the recursion unfolds, return
pointers, arguments, and intermediate results will be pushed on the
stack.  This continues until reaching the base case, at which point the
raise occurs.

Everything on the stack up to the handler should be discarded because
the raise discards the computation that has built up to this point:
all those pending function calls will never be returned to.  Instead,
control needs to transfer back to the @racket[with-handlers]
expression and execute the code that follows it.

A simple approach is to designate a register to hold a pointer to the
current handler on the stack.  When executing a @racket[raise], the
stack can be popped back to the handler, giving access to the
predicate, the function, and the return label used to jump back to the
@racket[with-handlers] expression.

This works when there is exactly one exception handler, but nested
handlers require one more piece of information: a pointer to the parent
handler.

So each handler can be represented by four things pushed on the stack:

@itemlist[
@item{a predicate,}
@item{a function,}
@item{a return label, and}
@item{a pointer to the parent handler on the stack, if there is one.}
]

A designated register can then hold a pointer to the current handler.

@section[#:tag-prefix "a9-" #:style 'unnumbered]{Submitting}

Submit a zip file containing your work to Gradescope.  Use
@tt{make submit.zip} from within the @tt{loot-exceptions} directory to
create a zip file with the proper structure.
