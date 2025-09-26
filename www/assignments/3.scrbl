#lang scribble/manual
@(require "../defns.rkt")
@title[#:tag "Assignment 3" #:style 'unnumbered]{Assignment 3: Primitives, conditionals}

@(require (for-label a86/ast (except-in racket ...)))

@bold{Due: @assign-deadline[3]}

The goal of this assignment is to extend the language developed in
@secref{Dupe} with some simple unary numeric and boolean operations
and a new form of control flow expressions: @racket[cond]-expressions.

@section[#:tag "a3-dupe-plus" #:style 'unnumbered]{Dupe+}

The Dupe+ language extends Dupe in the follow ways:

@itemlist[
@item{adding new primitive operations,}
@item{adding @racket[cond].}
]

@subsection[#:tag-prefix "a3-" #:style 'unnumbered]{Primitives}

The following new primitves are included in Dupe+:

@itemlist[
@item{@racket[(abs _e)]: compute the absolute value of @racket[_e],}
@item{@racket[(- _e)]: flip the sign of @racket[_e], i.e. compute @math{0-@racket[_e]}, and}
@item{@racket[(not _e)]: compute the logical negation of @racket[_e]; note that the negation of @emph{any} value other than @racket[#f] is @racket[#f] and the negation of @racket[#f] is @racket[#t].}
]

@subsection[#:tag-prefix "a3-" #:style 'unnumbered]{Conditional expressions}

The following new conditional form is included in Dupe+:

@racketblock[
(cond [_e-p1 _e-a1]
      ...
      [else _e-an])
]

A @racket[cond] expression has any number of clauses @racket[[_e-pi
_e-ai] ...], followed by an ``else'' clause @racket[[else _e-an]].
For the purposes of this assignment, we will assume every
@racket[cond] expression ends in an @racket[else] clause, even though
this is not true in general for Racket.  The parser will reject any
@racket[cond]-expression that does not end in @racket[else].


The meaning of a @racket[cond] expression is computed by evaluating
each expression @racket[_e-pi] in order until the first one that
does not evaluate to @racket[#f] is found, in which case, the corresponding expression
@racket[_e-ai] is evaluated and its value is the value of the
@racket[cond] expression.  If no such @racket[_e-pi] exists, the
expression @racket[_e-an]'s value is the value of the @racket[cond].


@section[#:tag-prefix "a3-" #:style 'unnumbered]{Implementing Dupe+}

You must extend the interpreter and compiler to implement Dupe+. (The
parser for Dupe+ is given to you.)  You are given a file
@tt{dupe-plus.zip} on ELMS with a starter compiler based on the
@secref{Dupe} language we studied in class.

You may use any a86 instructions you'd like, however it is possible to
complete the assignment using @racket[Cmp], @racket[Je], @racket[Jg],
@racket[Jmp], @racket[Label], @racket[Mov], and @racket[Sub].

@section[#:tag-prefix "a3-" #:style 'unnumbered #:tag "parse"]{Parsing Dupe+}

The AST type and parser for Dupe+ are given to you.

Here's the AST definition for the added primitives and @racket[cond]:

@#reader scribble/comment-reader
(racketblock
;; type Expr =
;; ...
;; | (Cond [Listof Expr] [Listof Expr] Expr)

;; type Op = 
;; ...
;; | 'abs | '- | 'not

(struct Cond (cs es el)    #:prefab)
)

There is one new kind of expression constructor: @racket[Cond].  A
@racket[Cond] AST node contains three parts: two equal length lists of
expression and an expression.  The two lists represent the clauses,
where the first list contains all of the left-hand-side parts of the
clauses and the other contains all of the right-hand-side parts.

Here are some examples of how concrete expressions are parsed into
ASTs using this representation:

@itemlist[

@item{@racket[(abs 1)] parses as @racket[(Prim1 'abs (Lit 1))],}

@item{@racket[(not #t)] parses as @racket[(Prim1 'not (Lit #t))],}

@item{@racket[(cond [else 5])] parses as @racket[(Cond '() '() (Lit 5))],}

@item{@racket[(cond [(not #t) 3] [else 5])] parses as @racket[(Cond
(list (Prim1 'not (Lit #t))) (list (Lit 3)) (Lit 5))],}

@item{@racket[(cond [(not #t) 3] [7 4] [else 5])] parses as
@racket[(Cond (list (Prim1 'not (Lit #t)) (Lit 7)) (list (Lit 3)
(Lit 4)) (Lit 5))],}
]

@section[#:tag-prefix "a3-" #:style 'unnumbered]{Steps toward Dupe+}

Implement the new expression forms as described earlier, both for the
interpreter and compiler.

To do this, you should:

@itemlist[
@item{Study @tt{ast.rkt} to understand how these new forms of
expression are represented.}

@item{Add test cases to @tt{test/test-runner.rkt}.  These will be
tested with both the interpreter and compiler.}

@item{Update @tt{interp-prim.rkt} and @tt{interp.rkt} to correctly
interpret @racket[cond] expressions and new primitives.}

@item{Test your interpreter with @tt{raco test test/interp.rkt}.}

@item{Make examples of @racket[cond]-expressions and primitives and
potential translations of them to assembly.}

@item{Update @tt{compile.rkt} and @tt{compile-ops.rkt} to correctly
compile these expressions based on your examples.}

@item{Test your compiler with @tt{raco test test/compile.rkt}.}
]

@section[#:tag-prefix "a3-" #:style 'unnumbered]{Submitting}

To submit, use @tt{make} from within the @tt{dupe-plus} directory to
create a zip file containing your work and submit it to Gradescope.
