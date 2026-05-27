#lang scribble/base
@(require racket/date
          racket/port
          racket/system
	  racket/string)

@(define (shell s)
   (with-output-to-string (λ () (system s))))

@(define (i s)
   (item (verbatim (shell s))))

@title[#:style '(unnumbered)]{Colophon}

This document (@string-trim[(shell "git rev-parse --short HEAD")]) was produced on @(date->string (current-date) #t).

System information:
@itemlist[
 (i "uname -srm")
 (i "racket --version")
 (i "raco pkg show -lu a86 langs")]
