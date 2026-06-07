#lang scribble/manual
@(require "../defns.rkt")
@title[#:tag "Assignment 1" #:style 'unnumbered]{Assignment 1: Getting started}

@(require (for-label a86/ast (except-in racket ...)))

@bold{Due: @assign-deadline[1]}

@bold{Starter code:} @link["code/blackmail-plus.zip"]{@tt{blackmail-plus.zip}}

The goal of this assignment is to extend the language developed in
@secref{Blackmail} with another simple unary numeric operation.  This
is a conceptually straightforward assignment, designed primarily to
make you familiar with the structure of the Blackmail language
implementation and the tools we will be using for the remainder of the
course.  It should not take long to complete, but may involve working
out some set-up issues.

@section[#:tag "p1-blackmail-plus" #:style 'unnumbered]{Blackmail+}

The Blackmail+ language extends Blackmail in the following ways:

@itemlist[
@item{adding a new primitive operation @racket[add2].}
]

Note: typically all of our languages are subsets of Racket, but this
is one of the exceptions since Racket doesn't actually have a built-in
@racket[add2] function.

@subsection[#:tag-prefix "p1-" #:style 'unnumbered]{Primitives}

The following new primitive is included in Blackmail+:

@itemlist[
@item{@racket[(add2 _e)]: add 2 to the value of @racket[_e].}
]

@section[#:tag-prefix "p1-" #:style 'unnumbered]{Implementing Blackmail+}

You must extend the interpreter and compiler to implement Blackmail+. (The
parser for Blackmail+ is given to you.)  Use the starter code in
@link["code/blackmail-plus.zip"]{@tt{blackmail-plus.zip}}, which is based on the
@secref{Blackmail} language we studied in class.

You may use any a86 instructions you'd like, however it is possible to
complete the assignment using just @racket[Add].

@section[#:tag-prefix "p1-" #:style 'unnumbered #:tag "parse"]{Parsing Blackmail+}

The AST type and parser for Blackmail+ are given to you.

Here's the AST definition for the added primitive:

@#reader scribble/comment-reader
(racketblock
;; type Expr =
;; ...

;; type Op = 
;; ...
;; | 'add2
)

There are no new kinds of expression constructors, but there
is a new @tt{Op} construtor: @racket['add2].

Here are some examples of how concrete expressions are parsed into
ASTs using this representation:

@itemlist[

@item{@racket[(add2 1)] parses as @racket[(Prim1 'add2 (Lit 1))],}

@item{@racket[(add2 (add2 1))] parses as @racket[(Prim1 'add2 (Prim1 'add2 (Lit 1)))],}

@item{@racket[(add2 (add1 1))] parses as @racket[(Prim1 'add2 (Prim1 'add1 (Lit 1)))].}
]

@section[#:tag-prefix "p1-" #:style 'unnumbered]{Steps toward Blackmail+}

Implement the new forms as described earlier, both for the interpreter
and compiler.

To do this, you should:

@itemlist[
@item{Study @tt{syntax/ast.rkt} to understand how these new forms of
expression are represented.}

@item{Add test cases to @tt{test/define-tests.rkt}.  These will be
tested with both the interpreter and compiler.}

@item{Update @tt{interpreter/interp-prim.rkt} to correctly interpret
the new primitives.  (You don't need to update
@tt{interpreter/interp.rkt}, but it would be a good idea to
familiarize yourself with this file.)}

@item{Test your interpreter with @tt{raco test
test/run-interp-tests.rkt}.}

@item{Make examples of the new primitive expressions and
potential translations of them to assembly.}

@item{Update @tt{compiler/compile-ops.rkt} to correctly compile these
expressions based on your examples. (You don't need to update
@tt{compiler/compile.rkt}, but again, it would be good to study this
file.)}

@item{Test your compiler with @tt{raco test test/run-compile-tests.rkt}.}
]

@section[#:tag-prefix "p1-" #:style 'unnumbered]{Submitting}

To submit, use @tt{make} from within the @tt{blackmail-plus} directory to
create a zip file containing your work and submit it to Gradescope.
