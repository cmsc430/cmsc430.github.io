#lang scribble/manual
@(require "../defns.rkt")
@title[#:tag "Assignment 4" #:style 'unnumbered]{Assignment 4: Case}

@(require (for-label a86/ast (except-in racket ...)))

@bold{Due: @assign-deadline[4]}

The goal of this assignment is to extend the language developed in
@secref{a3-dupe-plus} with another new form of control flow expressions:
@racket[case]-expressions.

@bold{Note:} you will need to carry forward all of the changes you
implemented for @secref{a3-dupe-plus}.

@section[#:tag-prefix "a4-" #:style 'unnumbered]{Dupe++}

The Dupe++ language extends @secref{a3-dupe-plus} in the follow ways:

@itemlist[
@item{adding @racket[case].}
]

@subsection[#:tag-prefix "a4-" #:style 'unnumbered]{Case expressions}

The following new case form is included in Dupe++:

@racketblock[
(case _ev
      [(_d1 ...) _e1]
      ...
      [else _en])
]

The @racket[case] expression form is a mechanism for dispatching
between a number of possible expressions based on a value, much like
C's notion of a @tt{switch}-statement.

The meaning of a @racket[case] expression is computed by evaluating
the expression @racket[_ev] and then proceeding in order through each
clause until one is found that has a datum @racket[_di] equal to
@racket[_ev]'s value.  Once such a clause is found, the corresponding
expression @racket[_ei] is evaluated and its value is the value of the
@racket[case] expression.  If no such clause exists, expression
@racket[_en] is evaluated and its value is the value of the
@racket[case] expression.

Note that each clause consists of a parenthesized list of
@emph{datums}, which in the setting of Dupe means either integer or
boolean literals.

For the purposes of this assignment, we will assume every
@racket[case] expression ends in an @racket[else] clause, even though
this is not true in general for Racket.  The parser will reject any
@racket[case]-expression that does not end in @racket[else].


@section[#:tag-prefix "a4-" #:style 'unnumbered]{Implementing Dupe++}

You must extend the interpreter and compiler to implement Dupe++.  You
are given a file @tt{dupe-plus-plus.zip} on ELMS with a starter
compiler based on the @secref{Dupe} language we studied in class.

You may use any a86 instructions you'd like, however it is possible to
complete the assignment using @racket[Cmp], @racket[Je], @racket[Jg],
@racket[Jmp], @racket[Label], @racket[Mov], and @racket[Sub].

@section[#:tag-prefix "a4-" #:style 'unnumbered #:tag "parse"]{Parsing Dupe++}

In the past, designing the AST type and structure definitions has
given students some grief.  Getting stuck at this point means you
can't make any progress on the assignment and making a mistake at this
level can cause real trouble down the line for your compiler.

For that reason, let us give you a strong hint for a potential design
of the ASTs and examples of how parsing could work.  You are not
required to follow this design, but you certainly may.

Here's the AST definition for @racket[case]:

@#reader scribble/comment-reader
(racketblock
;; type Expr = ...
;; | (Case Expr [Listof [Listof Datum]] [Listof Expr] Expr)

;; type Datum = Integer | Boolean

(struct Case (e ds cs el) #:prefab)
)

There is one new kind of expression constructor: @racket[Case].  A
@racket[Case] AST node contains four things: an expression that is the
subject of the dispatch (i.e. the expression that is evaluated to
determine which clause should be taken), a list of lists of datums, an
equal length list of expressions, and an @racket[else]-clause
expression.  A  @emph{datum} is either an integer or a boolean.

Here are some examples of how concrete expressions are parsed into
ASTs using this representation:

@itemlist[

@item{@racket[(case (add1 3) [else 2])] parses as @racket[(Case (Prim1
'add1 (Lit 3)) '() '() (Lit 2))].}

@item{@racket[(case 4 [(4) 1] [else 2])] parses as @racket[(Case (Lit
4) (list (list 4)) (list (Lit 1)) (Lit 2))],}

@item{@racket[(case 4 [(4 5 6) 1] [else 2])] parses as @racket[(Case (Lit
4) (list  (list 4 5 6)) (list (Lit 1)) (Lit 2))], and}

@item{@racket[(case 4 [(4 5 6) 1] [(#t #f) 7] [else 2])] parses as @racket[(Case (Lit
4) (list (list 4 5 6) (list #t #f)) (list (Lit 1) (Lit 7)) (Lit 2))].}
]


@section[#:tag-prefix "a4-" #:style 'unnumbered]{Implementing case}

Implement the @racket[case] expression form as described earlier.
To do this, you should:

@itemlist[
@item{Study @tt{ast.rkt} to understand how this new form of expression is represented.}

@item{Add test cases to @tt{test/test-runner.rkt}.  These will be
tested with both the interpreter and compiler.}

@item{Update @tt{interp.rkt} to correctly interpret @racket[case] expressions.}

@item{Bring forward all of the changes you made to the interpreter from @secref{a3-dupe-plus}.}

@item{Test your interpreter with @tt{raco test test/interp.rkt}.}

@item{Make examples of @racket[case]-expressions and potential translations of them
to assembly.}

@item{Update @tt{compile.rkt} to correctly compile @racket[case] expressions based on your examples.}

@item{Bring forward all of the changes you made to the compiler from @secref{a3-dupe-plus}.}

@item{Test your interpreter with @tt{raco test test/compile.rkt}.}

]

Note that only a small number of tests are given to you, so you should
write additional test cases.

@section[#:tag-prefix "a4-" #:style 'unnumbered]{Submitting}

To submit, use @tt{make} from within the @tt{dupe-plus-plus} directory to
create a zip file containing your work and submit it to Gradescope.
