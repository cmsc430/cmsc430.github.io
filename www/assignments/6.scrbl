#lang scribble/manual
@(require "../defns.rkt"
	  "../notes/ev.rkt"
	  "../notes/utils.rkt")
@title[#:tag "Assignment 6" #:style 'unnumbered]{Assignment 6: List and vector primitives}

@src-code["hoax-plus"]

@(require (for-label a86 (except-in racket ...)))

@(require racket/port)

@bold{Due: @assign-deadline[6]}

@(ev '(require hoax-plus))

The goal of this assignment is to gain proficiency with our
representation of memory-allocated values by implementing a number of
list and vector primitives.

@section[#:tag-prefix "a6-" #:style 'unnumbered]{Overview}

For this assignment, you are given a @tt{hoax-plus.zip} file on ELMS
with a starter compiler similar to the @seclink["Hoax"]{Hoax}
language we studied in class.


@section[#:tag-prefix "a6-" #:style 'unnumbered]{Hoax+}

The Hoax+ language extends the Hoax language we studied in class with four
new unary primitives:

@itemlist[
@item{@racket[length]: given a list, computes its length.}
@item{@racket[reverse]: given a list, computes a list with elements in the reverse order of the given list.}
@item{@racket[list->vector]: given a list, computes a vector with elements in the order of the given list.}
@item{@racket[vector->list]: given a vector, computes a list with elements in the order of the given vector.}
]

Unlike past assignments, you do not need to bring forward in any
features from earlier assignments.

@section[#:tag-prefix "a6-" #:style 'unnumbered]{List and vector primitves}


The new primitives have been added to the parser and the interpreter.
The behavior of compiled primitives should be consistent with the
interpreter:

@ex[
(interp (parse '(length '())))
(interp (parse '(length (cons 1 (cons 2 '())))))
(interp (parse '(length #f)))
(interp (parse '(reverse '())))
(interp (parse '(reverse (cons 1 (cons 2 '())))))
(interp (parse '(reverse #f)))
(interp (parse '(list->vector '())))
(interp (parse '(list->vector (cons 1 (cons 2 '())))))
(interp (parse '(list->vector #f)))
(interp (parse '(vector->list (make-vector 0 #t))))
(interp (parse '(vector->list (make-vector 2 #t))))
(interp (parse '(vector->list #f)))
]

The interpreter is consistent with Racket's own behavior, so if you're
unsure about what your compiler should do, you can look to Racket (or
the interpreter) for guidance:

@ex[
(length '())
(length (cons 1 (cons 2 '())))
(eval:error (length #f))
(reverse '())
(reverse (cons 1 (cons 2 '())))
(eval:error (reverse #f))
(list->vector '())
(list->vector (cons 1 (cons 2 '())))
(eval:error (list->vector #f))
(vector->list (make-vector 0 #t))
(vector->list (make-vector 2 #t))
(eval:error (vector->list #f))
]

@subsection[#:tag-prefix "a6-" #:style 'unnumbered]{Testing}

A small number of test cases have been provided in
@tt{test/test-runner.rkt}.  There is function called @racket[test]
that contains I/O-free test cases and another called @racket[test/io]
that contains I/O tests.  To run these tests, @tt{raco test
test/interp.rkt} will test the interpreter and @tt{raco test
test/compile.rkt} will test the compiler.  You are encouraged to add
your own tests.

@section[#:tag-prefix "a6-" #:style 'unnumbered]{Submitting}

To submit, use @tt{make} from within the @tt{hoax-plus} directory to
create a zip file containing your work and submit it to Gradescope.
