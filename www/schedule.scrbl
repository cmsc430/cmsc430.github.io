#lang scribble/manual
@(require scribble/core racket/list)
@(require "defns.rkt")

@title[#:style 'unnumbered]{Schedule}

@(define (wk d) (nonbreaking (bold d)))

@; for unreleased assignments, switch to seclink when ready to release
@(define (tbaseclink lnk txt) txt)

@(define (day s) @elem[s])


@tabular[#:style 'boxed
#:sep @hspace[1]
#:row-properties '(bottom-border)
(list (list @bold{Date} @bold{Topic} @bold{Notes} @bold{Due})
(list @day{6/1} "Intro to compilers, Racket language, a86 assembly" @itemlist[@item{@secref["Intro"]} @item{@secref["OCaml to Racket"]} @item{@secref["a86"]}]
      @seclink["Practice 1"]{P1})
(list @day{6/2} "First compiler, control flow, type tags" @itemlist[@item{@secref["Abscond"]} @item{@secref["Blackmail"]} @item{@secref["Con"]} @item{@secref["Dupe"]}] @seclink["Practice 2"]{P2})
(list @day{6/3} "I/O, system calls, ABI" @secref["Evildoer"] "")
(list @day{6/4} "Errors, type tag checking" @secref["Extort"] @seclink["Assignment 1"]{A1})
(list @day{6/5} "Binding and variables, run-time stack, compile-time environment" @secref["Fraud"] "")
(list @day{6/8} "Binary operations" @secref["Fraud"] @seclink["Assignment 2"]{A2})
(list @day{6/9} "Inductive data, memory allocation, pointer values" @secref["Hustle"] "")
(list @day{6/10} "Array data, pointer offsets, mutation" @secref["Hoax"] "")
(list @day{6/11} "Slack" "" @seclink["Assignment 3"]{A3})
(list @day{6/12} @bold{Exam, no lecture} "" @seclink["Exam_1"]{E1})
(list @day{6/15} "Function definitions and calls" @secref["Iniquity"] @seclink["Assignment 4"]{A4})
(list @day{6/16} "Tail calls" @secref["Jig"] "")
(list @day{6/17} "First-class functions, closures" @secref["Loot"] "")
(list @day{6/18} "First-class functions, code pointers, environments" @secref["Loot"] @seclink["Assignment 5"]{A5})
(list @day{6/19} @bold{Juneteenth Holiday, no lecture} "" "")
(list @day{6/22} "Pattern matching, interpretation" @secref["Knock"] @seclink["Assignment 6"]{A6})
(list @day{6/23} "Pattern matching, compilation" @secref["Knock"] "")
(list @day{6/24} "Symbols, static and dynamic interning" @secref["Mug"] "")
(list @day{6/25} "Compound static data" @secref["Mountebank"] @seclink["Assignment 7"]{A7})
(list @day{6/26} @bold{Exam, no lecture} "" @seclink["Exam_2"]{E2})
(list @day{6/29} "Structures" @secref["Neerdowell"] @seclink["Assignment 8"]{A8})
(list @day{6/30} "Slack" "" "")
(list @day{7/1} "Source transformations" "" "")
(list @day{7/2} "Peephole optimizations" "" @seclink["Assignment 9"]{A9})
(list @day{7/3} @bold{Independence Day Holiday, no lecture} "" "")
(list @day{7/6} "Garbage collection" "" @seclink["Assignment 10"]{A10})
(list @day{7/7} "Self-hosting" @secref["Outlaw"] "")
(list @day{7/8} "Conclusion" "" "")
(list @day{7/9} "Slack" "" "")
(list @day{7/10} @bold{Exam, no lecture} "" "E3")
)
]

@;{@bold{Final project assessment: @|final-date|.}}
