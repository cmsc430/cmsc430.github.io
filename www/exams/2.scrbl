#lang scribble/manual

@(require (for-label racket)
          "../notes/ev.rkt")

@(require "../defns.rkt")

@title{Exam 2}

@(define prefix "exam2-")

@bold{Due: @exam2-date @exam-end-time}

Exam 2 will be released at least @exam-hours hours prior to
its due date.

@section[#:tag-prefix prefix]{Practice}

There is a practice exam available on ELMS.  You may submit to the
Practice Exam 2 assignment on Gradescope to get feedback on your
solution.  However during the real exam, you will not get this
level of feedback from the autograder.  @bold{Make sure you do not
submit your practice exam solution for the real exam!  We will
not allow late submissions if you submit the wrong work.}

@section[#:tag-prefix prefix]{Instructions}

The exam will be released as a zip file on ELMS (see the
description of Exam 1 there for the link).

There are several parts to this exam.  Each part has its own
directory with a README and supplementary files.  Read the README in
each part for instructions on how to complete that part of the
exam.

@section[#:tag-prefix prefix]{Communications}

If you have questions about the exam, send a @bold{private} message on
@link[piazza]{Piazza}.

Answers to common clarifying questions will be posted to
Piazza.

If you have trouble reaching the course staff via Piazza, email
@tt{@prof1-email}.

You may not communicate with anyone outside of the course staff about
the exam.

@section[#:tag-prefix prefix]{Submissions}

You should submit your work as a single zip file of this directory on
Gradescope.  Unlike past assignments, Gradescope will only do a basic
test for well-formedness of your submission.  It will make sure the
directory layout is correct and that all the functions that will be
tested are available.  It will catch syntax errors in your code, but
it does not run any correctness tests.

If you fail these tests, we will not be able to grade your submission.
Passing these tests only means your submission is well-formed.  Your
actual grade will be computed after the deadline.

You are encouraged to check your own work.
