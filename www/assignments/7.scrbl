#lang scribble/manual
@(require "../defns.rkt"
	  "../notes/ev.rkt")

@title[#:tag "Assignment 7" #:style 'unnumbered]{Assignment 7:
Variable-arity and arity-overloaded functions}

@(require (for-label a86 (except-in racket ...)))

@bold{Due: @assign-deadline[7]}

@(ev '(require iniquity-plus))

The goal of this assignment is to implement variable arity functions.

@section[#:tag-prefix "a7-" #:style 'unnumbered]{Overview}

For this assignment, you are given a @tt{iniquity-plus.zip} file on ELMS
with a starter compiler similar to the @seclink["Iniquity"]{Iniquity}
language we studied in class.

@section[#:tag-prefix "a7-" #:style 'unnumbered #:tag "rest"]{Rest
arguments}

Many languages including JavaScript, C, C++, Java, Ruby, Go, PHP, and
Racket provide facilities for defining functions that take a ``rest
argument'' which allows the function to be called with more arguments
than expected and these additional arguments will be bound to a single
value that collects all of these arguments.  In Iniquity, as in
Racket, the obvious way of collecting these arguments into a single
value is to use a list.

Here are some examples:

@itemlist[

@item{@racket[(define (f . xs) ...)]: this function takes @emph{any} number
of arguments and binds @racket[xs] to a list containing all of them,}

@item{@racket[(define (f x . xs) ...)]: this function takes @emph{at
least} one argument and binds @racket[x] to the first argument and
@racket[xs] to a list containing the rest.  It's an error to call this function
with zero arguments.}

@item{@racket[(define (f x y z . xs) ...)]: this function takes
@emph{at least} three arguments and binds @racket[x], @racket[y], and
@racket[z] to the first three arguments and @racket[xs] to a list
containing the rest.  It's an error to call this function with 0, 1,
or 2 arguments.}
]

Here are some examples in Racket to get a sense of the behavior:

@ex[
(define (f . xs) (list xs))
(f)
(f 1)
(f 1 2)
(f 1 2 3)
(f 1 2 3 4)
(define (f x . xs) (list x xs))
(eval:error (f))
(f 1)
(f 1 2)
(f 1 2 3)
(f 1 2 3 4)
(define (f x y z . xs) (list x y z xs))
(eval:error (f))
(eval:error (f 1))
(eval:error (f 1 2))
(f 1 2 3)
(f 1 2 4)
]

The code generated for a function call should not change: it should
pass all of the arguments on the stack along with information about
the number of arguments.

The compilation of function definitions that use a rest argument
should generate code that checks that the given number of arguments is
acceptable and should generate code to pop all ``extra'' arguments off
the stack and construct a list which is then bound to the rest
parameter.

It is worth remembering that arguments are pushed on the stack in such
a way that the last argument is the element most recently pushed on
the stack.  This has the benefit of making it easy to pop off the
extra arguments and to construct a list with the elements in the
proper order.

HINT: the function definition knows the number of ``required''
arguments, i.e. the minimum number of arguments the function can be
called with---call this @math{m}---and the caller communicates how
many actual arguments have been supplied---call this @math{n}.  The
compiler needs to generate a loop that pops @math{n-m} times,
constructing a list with with popped elements, and then finally pushes
this list in order to bind it to the rest parameter.


@section[#:tag-prefix "a7-" #:style 'unnumbered #:tag "case-lambda"]{Arity dispatch}

Some languages such as Java, Haskell, and Racket make it possible to
overload a single function name with multiple definitions where the
dispatch between these different definitions is performed based on the
number (or kind) of arguments given at a function call.

In Racket, this is accomplished with the @racket[case-lambda] form for
constructing multiple-arity functions.

Here is an example:

@ex[
(define f
  (case-lambda
    [(x) "got one!"]
    [(p q) "got two!"]))

(f #t)
(f #t #f)
(eval:error (f #t #f 0))
]

This function can accept @emph{either} one or two arguments.  If given
one argument, it evaluates the right-hand-side of the first clause
with @racket[x] bound to that argument.  If given two arguments, it
evaluates the right-hand-side of the second clause with @racket[p] and
@racket[q] bound to the arguments.  If given any other number of
arguments, it signals an error.

A @racket[case-lambda] form can have any number of clauses (including
zero!) and the first clause for which the number of arguments is
acceptable is taken when the function is called.

Note that @racket[case-lambda] can be combined with rest arguments too.
A clause that accepts any number of arguments is written by simply
listing a parameter name (no parentheses).  A clause that accepts some
non-zero minimum number of parameters is written with a dotted
parameter list.

For example:

@ex[
(define f
  (case-lambda
    [(x y z . r) (length r)]
    [(x) "just one!"]))

(f 1 2 3 4 5 6)
(f #t)
(eval:error (f))
(eval:error (f 1 2))]

This function takes three or more arguments @emph{or} one argument.  Any
other number of arguments (i.e. zero or two) results in an error.

@ex[
(define f
  (case-lambda
    [(x y z) "three!"]
    [xs (length xs)]))

(f)
(f 1 2)
(f 1 2 3)
(f 1 2 3 4 5 6)
]

This function takes any number of arguments, but when given three, it
produces @racket["three!"]; in all other cases it produces the number
of arguments.

@section[#:tag-prefix "a7-" #:style 'unnumbered]{Representing the
syntax of function definitions}

The @seclink["Iniquity"]{Iniquity} language has a single function
definition form: @racket[(define (_f _x ...) _e)] which is represented
with the following AST type:

@#reader scribble/comment-reader
(racketblock
;; type Defn = (Defn Id (Listof Id) Expr)
(struct Defn (f xs e) #:prefab)
)

Because there are three different forms of function definition in
Iniquity+, we use the following AST representation:

@#reader scribble/comment-reader
(racketblock
;; type Defn = (Defn Id Fun)
(struct Defn (f fun) #:prefab)

;; type Fun = (FunPlain [Listof Id] Expr)
;;          | (FunRest [Listof Id] Id Expr)
;;          | (FunCase [Listof FunCaseClause])
;; type FunCaseClause = (FunPlain [Listof Id] Expr)
;;                    | (FunRest [Listof Id] Id Expr)
(struct FunPlain (xs e)   #:prefab)
(struct FunRest  (xs x e) #:prefab)
(struct FunCase  (cs)     #:prefab)
)

What used to be represented as @racket[(Defn _f _xs _e)] is now
represented as @racket[(Defn _f (FunPlain _xs _e))].


The parser already works for these new forms of function definitions.
Here are some examples of how function definitions are parsed, but you
are encouraged to try out more to get a better sense:

@ex[
(parse-define '(define (f x) x))
(parse-define '(define (f . xs) xs))
(parse-define '(define (f x y z . q) q))
(parse-define
  '(define f
     (case-lambda
       [(x y) 2]
       [(z) 1]
       [(a b c . d) "3+"]
       [q "other"])))
]

@section[#:tag-prefix "a7-" #:style 'unnumbered]{Starter code}

The compiler code given to you is just an implementation of Iniquity,
but updated to parse the new forms of function definitions and
re-organized slightly to match the new AST representation.

The interpreter code given to you works on the full Iniquity+
language, so you do not need to update @racket[interp.rkt] and can use
the interpreter to guide your implementation of the compiler.

@ex[
(interp
  (parse '(define (f x) x)
         '(f 1)))
(interp
  (parse '(define (f . x) x)
         '(f 1)))
(interp
  (parse '(define (f . x) x)
         '(f)))
(interp
  (parse '(define (f . x) x)
         '(f 1 2 3 4 5)))
(interp
  (parse '(define f
            (case-lambda
              [(x y) 2]
              [(z) 1]
              [(a b c . d) "3+"]
              [q "other"]))
         '(cons (f 7)
            (cons (f 3 4)
              (cons (f)
                (cons (f 7 8 9 10 11)
                      '()))))))
]


Thus, you should only need to modify @racket[compile.rkt].

A small number of test cases are given as usual.

@section[#:tag-prefix "a7-" #:style 'unnumbered]{Submitting}

To submit, use @tt{make} from within the @tt{iniquity-plus} directory
to create a zip file containing your work and submit it to Gradescope.
