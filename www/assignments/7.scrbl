#lang scribble/manual
@(require "../defns.rkt"
          "../notes/ev.rkt")

@title[#:tag "Assignment 7" #:style 'unnumbered]{Assignment 7:
Arity-overloaded functions}

@(require (for-label a86 (except-in racket ...)))

@bold{Due: @assign-deadline[7]}

@bold{Starter code:} @link["code/jig-plus.zip"]{@tt{jig-plus.zip}}

@(ev '(require jig-plus))

The goal of this assignment is to implement arity-overloaded functions.

@section[#:tag-prefix "a7-" #:style 'unnumbered]{Overview}

For this assignment, use the starter code in
@link["code/jig-plus.zip"]{@tt{jig-plus.zip}}, which is
similar to the @seclink["Jig"]{Jig} language we studied in
class.

You must extend the language with the features described here, plus
you must bring forward the features from @seclink["Assignment 6"]{Assignment 6}.

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

@section[#:tag-prefix "a7-" #:style 'unnumbered]{Representing overloaded definitions}

In this assignment, Iniquity+ adds a third kind of function
representation:

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

The parser already works for these new forms of function definitions.
Here is an example of how overloaded function definitions are parsed:

@ex[
(parse-define
  '(define f
     (case-lambda
       [(x y) 2]
       [(z) 1]
       [(a b c . d) "3+"]
       [q "other"])))
]

@section[#:tag-prefix "a7-" #:style 'unnumbered]{Starter code}

The compiler code given to you supports plain function definitions.
The interpreter code given to you works on the full Jig+
language, so you do not need to update @racket[interp.rkt] and can use
the interpreter to guide your implementation of the compiler.

@ex[
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

To submit, use @tt{make} from within the @tt{jig-plus} directory
to create a zip file containing your work and submit it to Gradescope.
