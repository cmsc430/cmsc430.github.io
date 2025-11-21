#lang scribble/manual
@(require "../defns.rkt")
@title[#:tag "Assignment 8" #:style 'unnumbered]{Assignment 8: Patterns}

@(require (for-label (except-in racket ...)))
@(require "../notes/ev.rkt"
          "../notes/utils.rkt")

@bold{Due: @assign-deadline[8]}

@(ev '(require knock-plus))

The goal of this assignment is to extend a compiler with new pattern
matching forms for matching lists, vectors, and predicates.

You are given a file @tt{knock-plus.zip} on ELMS with a starter
compiler similar to the @seclink["Knock"]{Knock} language we studied
in class.  You are tasked with:

@itemlist[

@item{implementing the @tt{list} pattern,}

@item{implementing the @tt{vector} pattern, and}

@item{implementing the @tt{?} pattern.}
]

The following files have already been updated for you @bold{and should
not be changed by you}:

@itemlist[ @item{@tt{ast.rkt}}
           @item{@tt{parse.rkt}}
	   @item{@tt{interp.rkt}}
           @item{@tt{interp-prim.rkt}}
           @item{@tt{compile-op.rkt}}	   
]

So you will only need to modify:
@itemlist[
@item{@tt{compile.rkt}}
]
to correctly implement the new features. These features are described below.

As a convenience, two new n-ary primitives have been added (and fully
implemented): @racket[list] and @racket[vector].  The @racket[list]
primitive takes any number of arguments and produces a list containing
the arguments as elements; the @racket[vector] primitive does the
same, but constructs a vector.

@ex[
(list)
(list 1 2 3)
(list 1 #t #\c)
(vector)
(vector 1 2 3)
(vector 1 #t #\c)]

These are not directly useful in implementing the patterns above, but
do make it easier to write examples and tests.

@section[#:tag-prefix "a8-" #:style 'unnumbered #:tag "list"]{List patterns}

The @racket[(list _p1 ... _pn)] pattern matches a list of elements.  The
pattern matches a list with as many elements as there are patterns
@racket[_p1] through @racket[_pn] and each element must match the
respective pattern.


@ex[
(match (list)
  [(list) #t]
  [_ #f])
(match (list 1 2 3)
  [(list x y z) x])
(match (list (list 1) (list 2))
  [(list (list x) (list 2)) x])
]

@section[#:tag-prefix "a8-" #:style 'unnumbered #:tag "vector"]{Vector patterns}

The @racket[(vector _p1 ... _pn)] pattern matches a vector of elements.  The
pattern matches a vector with as many elements as there are patterns
@racket[_p1] through @racket[_pn] and each element must match the
respective pattern.


@ex[
(match (vector)
  [(vector) #t]
  [_ #f])
(match (vector 1 2 3)
  [(vector x y z) x])
(match (vector (vector 1) (vector 2))
  [(vector (vector x) (vector 2)) x])
]

@section[#:tag-prefix "a8-" #:style 'unnumbered #:tag "vector"]{Predicate patterns}

The @racket[(? _f)] pattern matches any value for which the predicate
@racket[_f] returns a true value (any value other than @racket[#f])
when applied to the value being matched.  In Knock+, @racket[_f] must be
the name of a user defined function.

@ex[
(define (is-eight? x) (= x 8))
(define (id x) x)

(match 8
  [(? is-eight?) #t]
  [_ #f])
(match (vector 1 2 3)
  [(and (? id) x) x])
(match 16
  [(? is-eight?) #t]
  [_ #f])
]

@section[#:tag-prefix "a8-" #:style 'unnumbered]{Representing the syntax of patterns}

The AST of patterns is extended as follows:

@#reader scribble/comment-reader
(racketblock
;; type Pat  = ...
;;           | (List [Listof Pat])
;;           | (Vect [Listof Pat])
;;           | (Pred Id)
)

The parser includes a @racket[parse-pattern] function that parses a
single pattern:

@ex[
(parse-pattern 'x)
(parse-pattern '(cons x y))
(parse-pattern '(list x y z))
(parse-pattern '(vector x y z))
(parse-pattern '(? f?))
]



@section[#:tag-prefix "a8-" #:style 'unnumbered]{Submitting}

Submit a zip file containing your work to Gradescope.  Use @tt{make
submit.zip} from within the @tt{knock-plus} directory to create a zip
file with the proper structure.
