#lang scribble/manual
@(require "../defns.rkt")
@title[#:tag "Assignment 5" #:style 'unnumbered]{Assignment 5: Let There Be (Many) Variables}

@(require (for-label a86/ast (except-in racket ...)))

@bold{Due: @assign-deadline[5]}

The goal of this assignment is to extend a compiler with binding forms
that can take any number of arguments.

@section[#:tag-prefix "a5-" #:style 'unnumbered]{Overview}

For this assignment, you are given a @tt{fraud-plus.zip} file on ELMS
with a starter compiler similar to the @seclink["Fraud"]{Fraud}
language we studied in class.

@section[#:tag-prefix "a5-" #:style 'unnumbered]{Fraud+}

The Fraud+ language extends the Fraud language we studied in class with some
new features:

@itemlist[

@item{The features added in @seclink["Assignment 4"]{Assignment 4}, namely:

  @itemlist[

  @item{@racket[abs], @racket[-], and @racket[not]}
  @item{@racket[cond]}
  @item{@racket[case]}

  ]}

@item{New primitives @racket[integer?] and @racket[boolean?].}

@item{An extended @racket[let] form that can bind multiple variables at once.}

@item{Back-referencing @racket[let*] form that can bind multiple variables at once.}

]

@subsection[#:tag-prefix "a5-" #:style 'unnumbered]{From Dupe++ to Fraud+}

Implement the @racket[abs], unary @racket[-], and @racket[not] operations and
the @racket[cond] and @racket[case] forms from
@seclink["Assignment 4"]{Assignment 4} by modifying @tt{interp.rkt},
@tt{interp-prim.rkt}, @tt{compile.rkt}, and @tt{compile-ops.rkt}. You can
start from your previous code, but you will need to update it to work for the
code you are given. What's essentially left for you to do is to make sure to
correctly signal an error (@racket['err]) when these constructs are
applied to the wrong type of argument.

While you're at it, implement the predicates @racket[integer?] and
@racket[boolean?] for checking the type of an argument, modeled by the
@racket[char?] predicate that was covered in the lectures.

@subsection[#:tag-prefix "a5-" #:style 'unnumbered]{Generalizing Let}

The Fraud language has a @tt{let} form that binds a single variable in the
scope of some expression. This is a restriction of the more general form of
@racket[let] that binds any number of expressions. So, for example,

@racketblock[
(let ((x 1) (y 2) (z 3))
  _e)
]

simultaneously binds @racket[x], @racket[y], and @racket[z] in the scope of
@racket[_e].

The syntax of a @racket[let] expression allows any number of binders to occur,
so @racket[(let () _e)] is valid syntax and is equivalent to @racket[_e].

The binding of each variable is only in-scope within the body, @bold{not} in
the right-hand sides of any of the @racket[let]. So, for example,
@racketblock[(let ((x 1) (y x)) 0)] is a syntax error because the occurrence of
@racket[x] is not bound.

The provided interpreter and compiler work when the @racket[let]
expression happens to bind a single variable, but you must revise the
code to work for any number of bindings.


@subsection[#:tag-prefix "a5-" #:style 'unnumbered]{Back-Referencing Let}

Similar to @racket[let], there is also @racket[let*] that can also bind any
number of expressions. The difference is that previous bindings are available
in the right-hand sides of subsequent bindings. For example,

@racketblock[
(let* ((x 1) (y 2) (z (add1 y)))
  _e)
]

binds @racket[x] to 1, @racket[y] to 2, and @racket[z] to 3 in
the scope of @racket[_e].

The syntax of a @racket[let*] expression allows any number of binders to occur,
so @racket[(let* () _e)] is valid syntax and is equivalent to @racket[_e].

Unlike @racket[let], @racketblock[(let* ((x 1) (y x)) 0)] is @emph{not} a
syntax error. However, bindings are only available forward, so
@racketblock[(let* ((x y) (y 1)) 0)] @emph{is} a syntax error.

The provided interpreter and compiler work when the @racket[let*]
expression happens to bind a single variable, but you must revise the
code to work for any number of bindings.

@section[#:tag-prefix "a5-" #:style 'unnumbered]{Submitting}

To submit, use @tt{make} from within the @tt{fraud-plus} directory to
create a zip file containing your work and submit it to Gradescope.
