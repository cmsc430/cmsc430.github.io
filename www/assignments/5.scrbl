#lang scribble/manual
@(require "../defns.rkt")
@title[#:tag "Assignment 5" #:style 'unnumbered]{Assignment 5: When and unless}

@(require (for-label a86 (except-in racket ...)))

@bold{Due: @assign-deadline[5]}

Details of this assignment will be released later in the semester.

@;{
The goal of this assignment is to extend the language developed in
@secref{Extort} with new forms of control flow expressions:
@racket[when]- and @racket[unless]-expressions.

@section[#:tag-prefix "a5-" #:style 'unnumbered]{Extort+}

The Extort+ language extends Extort in the follow ways:

@itemlist[
@item{adding @racket[when],}
@item{adding @racket[unless], and}
@item{bringing forward all the features of Dupe+.}
]


@section[#:tag-prefix "a5-" #:style 'unnumbered]{Testing}

You can test your code in several ways:

@itemlist[

 @item{Using the command line @tt{raco test .} from
  the directory containing the repository to test everything.}

 @item{Using the command line @tt{raco test <file>} to
  test only @tt{<file>}.}
]

Note that only a small number of tests are given to you, so you should
write additional test cases.

@section[#:tag-prefix "a5-" #:style 'unnumbered]{Submitting}

To submit, use @tt{make} from within the code directory to create a
zip file containing your work and submit it to Gradescope.
}