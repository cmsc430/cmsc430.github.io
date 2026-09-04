#lang scribble/manual
@(require scribble/core racket/list racket/format)
@(require "defns.rkt")

@title[#:style 'unnumbered]{Schedule}

@(define (wk d) (nonbreaking (bold d)))

@; for unreleased assignments, switch to seclink when ready to release
@(define (tbaseclink lnk txt) txt)

@(define (day s) @elem[s])
@(define (day/slide n s)
  (define (slide-url n)
    (format "slides/cmsc430-~a-~a-~a.pdf"
            year
	    semester
            (~r n #:min-width 2 #:pad-string "0")))
  @elem{@link[(slide-url n) s]})

@tabular[#:style 'boxed
         #:sep @hspace[1]
         #:row-properties '(bottom-border)
	 (list (list @bold{Date} @bold{Topic} @bold{Notes} @bold{Due})
	 
               (list @day/slide[1]{9/1}
	             "Intro to compilers, Racket language"
                     @itemlist[@item{@secref["Intro"]}
                               @item{@secref["OCaml to Racket"]}]
                     @seclink["Practice 1"]{P1})

               (list @day/slide[2]{9/3}
	             "Assembly, first compiler"
                     @itemlist[@item{@secref["a86"]}
		               @item{@secref["Abscond"]}]
	             @seclink["Practice 2"]{P2})


               (list @day{9/8}
	             "Unary primitives, control flow"
                     @itemlist[@item{@secref["Blackmail"]}
			       @item{@secref["Con"]}]
	             "")

	       (list @day{9/10}
	             "Disjoint datatypes, I/O, system calls, ABI"
                     @itemlist[@item{@secref["Dupe"]}
                               @item{@secref["Dodger"]}
                               @item{@secref["Evildoer"]}]
	             @seclink["Assignment 1"]{A1})
		     
	             
               (list @day{9/15}
	             "Errors, type tag checking"
                     @secref["Extort"]
		     "")

               (list @day{9/17}
	             "Binding and variables, run-time stack, compile-time environment"
		     @secref["Fraud"]
		     @seclink["Assignment 2"]{A2})
		     
               (list @day{9/22}
	             "Binary operations"
                     @secref["Fraud"]
                     "")

               (list @day{9/24}
	             "Inductive data, memory allocation, pointer values"
                     @secref["Hustle"]		     
		     @seclink["Assignment 3"]{A3})

               (list @day{9/29}
	             "Inductive data, memory allocation, pointer values"
		     @secref["Hustle"]
		     "")

               (list @day{10/1}	             
                     @seclink["Exam_1"]{Exam 1}
		     ""
		     @seclink["Exam_1"]{E1})

               (list @day{10/6}
	       	     "Array data, pointer offsets, mutation"
		     @secref["Hoax"]
                     "")

	       (list @day{10/8}
	             "Strings"
		     @secref["Hoax"]
		     @seclink["Assignment 4"]{A4})
		      
               (list @day{10/13}
                     @elem{No class: Fall Break}
		     ""
		     "")

	       (list @day{10/15}
                     @elem{slack}
		     ""
		     "")

               (list @day{10/20}
	             "Function definitions and calls"
                     @secref["Iniquity"]
		     "")
               (list @day{10/22}
	             "Function definitions and calls"
                     @secref["Iniquity"]
		     "")
                     
               (list @day{10/27}
	             "Tail calls"
                     @secref["Jig"]
		     "")
	       (list @day{10/29}
	             "Pattern matching, interpretation"
		     @secref["Knock"]
		     "")

               (list @day{11/3}
	             "Pattern matching, compilation"
                     @secref["Knock"]
		     "")

               (list @day{11/5}
                     @seclink["Exam_2"]{Exam 2}
		     ""
		     @seclink["Exam_2"]{E2})
		     

               (list @day{11/10}
	             "Lambda"
	       	     @secref["Loot"]
                     "")

               (list @day{11/12}
	             "Lambda"
		     @secref["Loot"]
		     "")

               (list @day{11/17}
                     "Symbols, static and dynamic interning"
                     @secref["Mug"]
                     "")

               (list @day{11/19}
                     "Compound static data"
		     @secref["Mountebank"]
		     "")

               (list @day{11/24}
                     @elem{Slack}
		     ""
		     "")
		     
	       (list @day{11/26}	             
                     @elem{No class: Thanksgiving}
		     ""
		     "")

               (list @day{12/1}
	             "Structures"
                     @secref["Neerdowell"]
                     "")

	       (list @day{12/3}
	             "Self-hosting"
                     @secref["Outlaw"]
		     "")

               (list @day{12/8}
	             "Self-hosting"
		     @secref["Outlaw"]		     
		     "")

               (list @day{12/11}             
                     @elem{Slack}
		     ""
		     ""))]

@bold{Final exam: @|final-date|, @|final-time|.}
