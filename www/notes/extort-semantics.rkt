#lang racket

(provide E-concrete E 𝑬)

(require redex/reduction-semantics
         (only-in "dupe-semantics.rkt" D-concrete D 𝑫))

(define-extended-language E-concrete D-concrete
  (e ::= ....)
  (a ::= v err))

(define-extended-language E D
  (e ::= ....)
  (a ::= v err))

(define-extended-judgment-form E 𝑫
  #:mode (𝑬 I O)
  #:contract (𝑬 e a)
  [(𝑬 e b)
   --------
   (𝑬 (Prim1 'add1 e) err)]

  [(𝑬 e b)
   -----------
   (𝑬 (Prim1 'sub1 e) err)]

  [(𝑬 e b)
   -----------
   (𝑬 (Prim1 'zero? e) err)]

  [(𝑬 e err)
   -----------
   (𝑬 (Prim1 'zero? e) err)]

  [(𝑬 e err)
   -----------
   (𝑬 (Prim1 'add1 e) err)]

  [(𝑬 e err)
   -----------
   (𝑬 (Prim1 'sub1 e) err)]

  [(𝑬 e err)
   -----------
   (𝑬 (If e e_0 e_1) err)])
