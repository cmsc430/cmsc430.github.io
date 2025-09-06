#lang scribble/base
@(require racket/date
          racket/port
          racket/system
	  racket/string)

@(define (shell s)
   (with-output-to-string (λ () (system s))))

@(define (i s)
   (item (verbatim (shell s))))

@title{Colophon}

This document (@string-trim[(shell "git rev-parse --short HEAD")]) was produced on @(date->string (current-date) #t).

System information:
@itemlist[
 (i "uname -a")
 (i "racket --version")
 (i "raco pkg show -u a86 langs")
 (i "nasm --version")
 (i "gcc --version")]
