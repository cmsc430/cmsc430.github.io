#lang scribble/manual
@(require "../defns.rkt")
@title[#:tag "Assignment 3" #:style 'unnumbered]{Assignment 3: Case}

@(require (for-label a86/ast (except-in racket ...)))

@bold{Due: @assign-deadline[3]}

@bold{Starter code:} @link["code/extort-plus.zip"]{@tt{extort-plus.zip}}

The goal of this assignment is to extend the language developed in
@secref{Extort} to add a new form of control flow expressions,
@racket[case]-expressions, and to bring the features developed in
@secref{a2-dupe-plus} to Extort.

@bold{Note:} you will need to carry forward all of the changes you
implemented for @secref{a2-dupe-plus} and adapt them to the setting of
Extort.

@section[#:tag-prefix "a3-" #:style 'unnumbered]{Extort+}

The Extort+ language extends @secref{Extort} in the follow ways:

@itemlist[
@item{adding @racket[case],}
@item{adding @racket[cond],}
@item{adding unary primitives @racket[-], @racket[abs], and @racket[not].}
]

@subsection[#:tag-prefix "a3-" #:style 'unnumbered]{Case expressions}

The following new case form is included in Extort+:

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
@emph{datums}, which in the setting of Extort means integer,
boolean, and character literals.

As a convenience, @racket[case] expressions can omit the @racket[else]
clause, but the parser will insert a final @racket[else] clause that
evaluates to @racket[(void)], so both the interpreter and compiler may
always assume the presence of an @racket[else]-clause.  (The same
convenience has been extended to @racket[cond] by the parser.)

@section[#:tag-prefix "a3-" #:style 'unnumbered]{Implementing Extort+}

You must extend the interpreter and compiler to implement Extort+.  Use
the starter code in
@link["code/extort-plus.zip"]{@tt{extort-plus.zip}}, which is
based on the @secref{Extort} language we studied in class.

You may use any a86 instructions you'd like.

@section[#:tag-prefix "a3-" #:style 'unnumbered #:tag "parse"]{Parsing Extort+}

The AST type and parser for Extort+ are given to you.

Here's the AST definition for the added primitives, @racket[cond], and
@racket[case]:

@#reader scribble/comment-reader
(racketblock
;; type Expr = ...
;; | (Cond [Listof Expr] [Listof Expr] Expr)
;; | (Case Expr [Listof [Listof Datum]] [Listof Expr] Expr)

;; type Datum = Integer | Boolean | Character

;; type Op = 
;; ...
;; | 'abs | '- | 'not

(struct Cond (cs es el)   #:prefab)
(struct Case (e ds cs el) #:prefab)
)

There are two new kinds of expression constructors: @racket[Case] and
@racket[Cond]. The @racket[Cond] form is unchanged from
@secref{a2-dupe-plus}.  A @racket[Case] AST node contains four things:
an expression that is the subject of the dispatch (i.e. the expression
that is evaluated to determine which clause should be taken), a list
of lists of datums, an equal length list of expressions, and an
@racket[else]-clause expression.  A @emph{datum} is either an integer,
a boolean, or a character, i.e. it includes all of the literals
allowed in Extort.

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


@section[#:tag-prefix "a3-" #:style 'unnumbered]{Implementing case}

Implement the @racket[case] expression form as described earlier.
To do this, you should:

@itemlist[
@item{Study @tt{syntax/ast.rkt} to understand how this new form of expression is represented.}

@item{Add test cases to @tt{test/define-tests.rkt}.  These will be
tested with both the interpreter and compiler.}

@item{Update @tt{interpreter/interp.rkt} to correctly interpret @racket[case] expressions.}

@item{Bring forward all of the changes you made to the interpreter from @secref{a2-dupe-plus}.}

@item{Test your interpreter with @tt{raco test test/run-interp-tests.rkt}.}

@item{Make examples of @racket[case]-expressions and potential translations of them
to assembly.}

@item{Update @tt{compiler/compile.rkt} to correctly compile @racket[case] expressions based on your examples.}

@item{Bring forward all of the changes you made to the compiler from @secref{a2-dupe-plus}.}

@item{Test your interpreter with @tt{raco test test/run-compile-tests.rkt}.}

]

Note that only a small number of tests are given to you, so you should
write additional test cases.

@section[#:tag-prefix "a3-" #:style 'unnumbered]{Submitting}

To submit, use @tt{make} from within the @tt{extort-plus} directory to
create a zip file containing your work and submit it to Gradescope.
