#lang scribble/manual
@(require scribble/core
	  scriblib/footnote
          scribble/decode
          scribble/html-properties
      	  "defns.rkt"
          "utils.rkt")

@(define (blockquote . strs)
   (make-nested-flow (make-style "blockquote" '(command))
                     (decode-flow strs)))


@(define accessible
   (style #f (list (js-addition "js/accessibility.js")
                   (attributes '((lang . "en"))))))

@title[#:style accessible @courseno]{: Design and Implementation of Programming Languages}

@image[#:scale 1/2 #:style float-right]{img/wizard.png}

@emph{@string-titlecase[semester], @year}

@emph{Lecture}: @lecture-schedule1, @classroom1

@emph{Professor}: @prof1


CMSC 430 is an introduction to compilers.  Its major goal is to arm
students with the ability to design, implement, and extend a
programming language. Throughout the course, students will design and
implement several related languages.


@tabular[#:style 'boxed
         #:row-properties '(bottom-border ())
	 (list* (list @bold{Staff} 'cont)
	        (list @bold{Name} @elem{@bold{E-mail}})
	        (list prof1 prof1-email)
	        #;(list prof2 prof2-email)
		staff)]


@;{
@tabular[#:style 'boxed
         #:row-properties '(bottom-border ())
	 (list (list @bold{Time}  @bold{Monday} @bold{Tuesday} @bold{Wednesday} @bold{Thursday}  @bold{Friday})
               (list      "8 AM"  'cont        'cont           'cont            'cont         'cont)
	       (list      "9 AM"  'cont        'cont           'cont            'cont         'cont)
	       (list     "10 AM"  'cont        'cont           'cont            'cont         'cont)
	       (list     "11 AM"  'cont        'cont           'cont            'cont         'cont)
	       (list     "12 PM"  'cont        'cont           'cont            'cont         'cont)
	       (list      "1 PM"  'cont        'cont           'cont            'cont         'cont)
	       (list      "2 PM"  'cont        'cont           'cont            'cont         'cont)
	       (list      "3 PM"  'cont        'cont           'cont            'cont         'cont)
	       (list      "4 PM"  'cont        'cont           'cont            'cont         'cont)
	       (list      "5 PM"  'cont        'cont           'cont            'cont         'cont))]
}

@bold{Communications:} @link[@elms-url]{ELMS}, @link[@piazza]{Piazza}

@bold{Assumptions:} This course assumes you know the material in CMSC 330 and
CMSC 216. In particular, you need to know how to program in a functional
programming language like OCaml and some familiarity with programming in C and
Assembly. See the @seclink["Texts"]{Texts} page for references to brush up on
this material.

@bold{Disclaimer:} All information on this web page is tentative and subject to
change. Any substantive change will be accompanied with an announcement to the
class via ELMS.

@bold{Feedback:} We welcome anonymous feedback on the course and its
staff using this @link[feedback]{form}.

@bold{TA office hours:} @office-hour-location

@itemlist[
 #:style 'compact
 @item{Monday
  @itemlist[
   @item{9:00–12:00 — Zhonqi}
   @item{12:00–3:00 — Ben}
   @item{3:30–6:30 — Kalyan}]}
 @item{Tuesday}
 @item{Wednesday
  @itemlist[
   @item{9:00–12:00 — Zhonqi}
   @item{12:00–3:00 — Ben}
   @item{3:30–6:30 — Kalyan}]}
 @item{Thursday
  @itemlist[
   @item{10:00–1:00 — Zinat}]}
 @item{Friday
  @itemlist[
   @item{11:00–2:00 — Zinat}
   @item{1:00–4:00 — Kalyan}]}
]     

@include-section{syllabus.scrbl}
@include-section{texts.scrbl}
@include-section{schedule.scrbl}
@include-section{notes.scrbl}
@include-section{assignments.scrbl}
@include-section{midterms.scrbl}
@include-section{project.scrbl}
@include-section{software.scrbl}
