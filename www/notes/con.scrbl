#lang scribble/manual

@(require (for-label (except-in racket compile ...) a86/printer a86/ast a86/registers))
@(require redex/pict
          racket/runtime-path
          scribble/examples
	  #;(except-in "con/semantics.rkt" ext lookup)
	  #;(prefix-in sem: (only-in "con/semantics.rkt" ext lookup))
	  "utils.rkt"
	  "ev.rkt"
	  "con-semantics.rkt"
	  "../utils.rkt")



@(define codeblock-include (make-codeblock-include #'h))


@(ev '(require rackunit a86))
@(ev '(require con/compiler/compile))
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path langs "con" f))))))
	   '("main.rkt" "syntax/random.rkt" "correct.rkt"))


@title[#:tag "Con"]{Con: branching with conditionals}

@src-code["con"]

@emph{When you come to a fork in the road, take it.}

@table-of-contents[]

@section{Conditional execution}

Let's now consider adding a notion of @bold{conditionals} to our target
language.

We'll call it @bold{Con}.

We will use the following concrete syntax: @racket[(if (zero? _e0) _e1 _e2)].

This leads to the following grammar for concrete Con:

@centered{@render-language[C-concrete]}

And abstract grammar:

@codeblock-include["con/syntax/ast.rkt"]

@;{
 We will also need a predicate for well-formed Con expressions, but
let's return to this after considering the semantics and interpreter.
}

The parser is similar to what we've seen before:

@codeblock-include["con/syntax/parse.rkt"]

@ex[
(parse '(if (zero? 42) 1 2))
(parse '(if (zero? (sub1 1)) (add1 2) (sub1 7)))
(parse '(if (zero? 0) (if (zero? 1) 2 3) 4))]


@section{Meaning of Con programs}

The meaning of Con programs depends on the form of the expression and
the new form is an if-expression.

@itemlist[

@item{the meaning of a if expression @racket[(IfZero _e0 _e1 _e2)] is
the meaning of @racket[_e1] if the meaning of @racket[_e0] is 0 and is
the meaning of @racket[_e2] otherwise.}

]

Let's consider some examples (using concrete notation):

@itemlist[

@item{@racket[(if (zero? 0) (add1 2) 4)] means @racket[3].}
@item{@racket[(if (zero? 1) (add1 2) 4)] means @racket[4].}
@item{@racket[(if (zero? (if (zero? (sub1 1)) 1 0)) (add1 2) 4)] means @racket[4].}
@item{@racket[(if (zero? (add1 0)) (add1 2) (if (zero? (sub1 1)) 1 0))] means @racket[1].}

]


@;{The semantics is inductively defined as before.  There are @emph{two}
new rules added for handling if-expressions: one for when the test
expression means @racket[0] and one for when it doesn't.

@(define ((rewrite s) lws)
   (define lhs (list-ref lws 2))
   (define rhs (list-ref lws 3))
   (list "" lhs (string-append " " (symbol->string s) " ") rhs ""))

@(require (only-in racket add-between))
@(define-syntax-rule (show-judgment name i j)
   (with-unquote-rewriter
      (lambda (lw)
        (build-lw (lw-e lw) (lw-line lw) (lw-line-span lw) (lw-column lw) (lw-column-span lw)))
      (with-compound-rewriters (['+ (rewrite '+)]
                                ['- (rewrite '–)]
                                ['= (rewrite '=)]
				['!= (rewrite '≠)])
        (apply centered
	   (add-between 
             (build-list (- j i)
	                 (λ (n) (begin (judgment-form-cases (list (+ n i)))
	                               (render-judgment-form name))))
             (hspace 4))))))

@(show-judgment 𝑪 0 3)
@(show-judgment 𝑪 3 5)


@;{
The heart of the semantics is an auxiliary relation, @render-term[C
𝑪𝒓], which relates an expression and an environement to the integer
the expression evaluates to (in the given environment):

@(define ((rewrite s) lws)
   (define lhs (list-ref lws 2))
   (define rhs (list-ref lws 3))
   (list "" lhs (string-append " " (symbol->string s) " ") rhs ""))

@(require (only-in racket add-between))
@(define-syntax-rule (show-judgment name i j)
   (with-unquote-rewriter
      (lambda (lw)
        (build-lw (lw-e lw) (lw-line lw) (lw-line-span lw) (lw-column lw) (lw-column-span lw)))
      (with-compound-rewriters (['+ (rewrite '+)]
                                ['- (rewrite '–)])
        (apply centered
	   (add-between 
             (build-list (- j i)
	                 (λ (n) (begin (judgment-form-cases (list (+ n i)))
	                               (render-judgment-form name))))
             (hspace 4))))))

@(show-judgment 𝑪𝒓 0 3)
@(show-judgment 𝑪𝒓 3 5)

It relies on two functions: one for extending an environment with a
variable binding and one for lookup up a variable binding in an
environment:

@centered{
@render-metafunction[sem:ext #:contract? #t]

@(with-atomic-rewriter
  'undefined
  "⊥"
  (render-metafunction sem:lookup #:contract? #t))}

The operational semantics for Con is then defined as a binary relation
@render-term[C 𝑪], which says that @math{(e,i)} in @render-term[C 𝑪],
only when @math{e} evaluates to @math{i} in the empty environment
according to @render-term[C 𝑪𝒓]:

@(show-judgment 𝑪 0 1)
}
}

The semantics is defined by extending the interpreter to add a case
for if-expressions, which recursively evaluates the test expression
and branches based on its value.

@codeblock-include["con/interpreter/interp.rkt"]

We can confirm the interpreter computes the right result for the
examples given earlier (using @racket[parse] to state the examples
with concrete notation):

@ex[
(interp (parse '(if (zero? 0) (add1 2) 4)))
(interp (parse '(if (zero? 1) (add1 2) 4)))
(interp (parse '(if (zero? (if (zero? (sub1 1)) 1 0)) (add1 2) 4)))
(interp (parse '(if (zero? (add1 0)) (add1 2) (if (zero? (sub1 1)) 1 0))))
]


@section{An Example of Con compilation}

Suppose we want to compile @racket[(if (zero? 8) 2 3)]...

We already know how to compile the @racket[8], @racket[2], and
@racket[3] part.

What needs to happen?

@itemlist[
@item{Execute the code for @racket[8] leaving the result in @racket[rax],}
@item{check whether @racket[rax] holds zero,}
@item{if it does, execute the code for @racket[2],}
@item{if it doesn't, execute the code for @racket[3].}
]

We can determine whether @racket[8] evaluates to @racket[0] using a
comparison instruction: @racket[(Cmp rax 0)].  To do the conditional
execution, we will need to jump to different parts of the code to
either execute the code for @racket[2] or @racket[3].  There are
several ways we could accomplish this, but we take the following
approach: immediately after the comparison, do a conditional jump to
the code for the else branch when not zero, otherwise fall through to
the code for the then branch.  At the end of the then branch,
unconditionally jump to the end of the code.

To accomplish this, we will need two labels: one for the else
branch code and one for the end of the code.


In total, the code for this example would look like:

@racketblock[
(list (Mov rax 8)
      (Cmp rax 0)
      (Jne 'else)
      (Mov rax 2)
      (Jmp 'end)
      (Label 'else)
      (Mov rax 3)
      (Label 'end))
]

Since @racket[(if (zero? 8) 2 3)] evaluates to @racket[3], we expect
that this code will leave @racket[3] in the @racket[rax] after
executing.  Step through the code in your mind and try to confirm
this.  After that, you can run the code using @racket[asm-interp]
to confirm mechanically.

@ex[
(asm-interp
  (prog (Global 'entry)
        (Label 'entry)
	(Mov rax 8)
        (Cmp rax 0)
        (Jne 'else)
        (Mov rax 2)
        (Jmp 'end)
        (Label 'else)
        (Mov rax 3)
        (Label 'end)
	(Ret)))]

This does indeed seem to do the right thing.  If changed the example
to @racket[(if (zero? 0) 2 3)] we would expect to get @racket[2].  We
can revise the code by changing what we initially put in @racket[rax]
and confirm:

@#reader scribble/comment-reader
(ex
(asm-interp
  (prog (Global 'entry)
        (Label 'entry)
	(Mov rax 0) ; changed from 8 to 0
        (Cmp rax 0)
        (Jne 'else)
        (Mov rax 2)
        (Jmp 'end)
        (Label 'else)
        (Mov rax 3)
        (Label 'end)
	(Ret)))
)

From these two concrete examples, it would seem we're well on our way
to having a compiler.  The way to compile @racket[(if (zero? _e0) _e1
_e2)] is:

@racketblock[
(seq (compile-e _e0)
     (Cmp rax 0)
     (Jne 'else)
     (compile-e _e1)
     (Jmp 'end)
     (Label 'else)
     (compile-e _e2)
     (Label 'end))
]

But there is a lurking problem here.  See if you can spot it.

Although our sketch works for the simple examples we used, a more
compilicated example can demonstrate this compiler is incorrect.  In
fact, we can write examples that will cause the compiler to error
instead of producing code, which definitely violates our specification
of correctness.

The issue is that the compiler needs to be correct for all possible
subexpressions @racket[_e0], @racket[_e1], and @racket[_e2].  While it
works for things like @racket[8], @racket[2], and @racket[3], it will
not work for certain kinds of subexpressions, namely subexpressions
that themselves include conditional expressions.  To see write out
what you expect @racket[(if (zero? 8) (if (zero? 0) 2 3) 3)] should
compile to.  What do you notice?


@section{A Compiler for Con}

The problem with our sketch for compiling conditional expressions is
the choice of label names.  We used @racket['else] and @racket['end]
as labels that get defined in the output of compiling a conditional.
This works fine so long as there's only one conditional expression,
but the moment there is more than one, we have label confusion: there
will be multiple ambiguous definitions of @racket['else] and
@racket['end].  The assembly will reject such programs and
@racket[prog] will error.  This is a real bug because clearly nested
conditionals are valid Con expressions and our interpreter handles
them just fine, so our compiler is obligated to produce correct code.
In this case, the compiler doesn't even produce code, much less
correct code.

The fix is to generate new label names ever time we compile a
conditional.  These fresh names will avoid clashes.  In Racket, the
@racket[gensym] function can be used to generate symbols that have not
appeared before, which is exactly the property we need here.  What
symbol will Racket choose?  We can try things out in the REPL to see.

@ex[
(gensym)
(gensym)
(gensym)
]

You can see that Racket is just giving us arbitrary symbol names, but
they have the important property that are unique.

Although not technically necessary it might be nice to make the
symbols more readable by telling @racket[gensym] a prefix to use:

@ex[
(gensym 'ifz)
(gensym 'ifz)]

This way we can leave hints in the assembly code about where the
labels came from.

Now for our compiler, we can do the following:

@racketblock[
(let ((l1 (gensym 'ifz))
      (l2 (gensym) 'ifz))
  (seq (compile-e _e0)
       (Cmp rax 0)
       (Jne l1)
       (compile-e _e1)
       (Jmp l2)
       (Label l1)
       (compile-e _e2)
       (Label l2)))
]

This will generate a new symbol and bind it to the @emph{variable}
@racket[l1] and similiarly for @racket[l2].

We're now ready to write the complete compiler for Con.

@codeblock-include["con/compiler/compile.rkt"]

Let's take a look at a few examples:
@ex[
(define (show s)
  (compile-e (parse s)))
    
(show '(if (zero? 8) 2 3))
(show '(if (zero? 0) 1 2))
(show '(if (zero? 0) (if (zero? 0) 8 9) 2))
(show '(if (zero? (if (zero? 2) 1 0)) 4 5))
]

And confirm they are running as expected:
@ex[
(asm-interp (compile (parse '(if (zero? 8) 2 3))))
(asm-interp (compile (parse '(if (zero? 0) 1 2))))
(asm-interp (compile (parse '(if (zero? 0) (if (zero? 0) 8 9) 2))))
(asm-interp (compile (parse '(if (zero? (if (zero? 2) 1 0)) 4 5))))
]


@section[#:tag-prefix "con"]{Correctness and random testing}

The statement of correctness follows the same outline as before:

@bold{Compiler Correctness}: @emph{For all @racket[e] @math{∈} @tt{Expr},
@racket[(interp e)] equals @racket[(asm-interp (compile e))].}

Again, we formulate correctness as a property that can be tested:

@codeblock-include["con/correct.rkt"]

Generating random Con programs is essentially the same as Blackmail
programs, and are provided in a @link["con/random.rkt"]{random.rkt}
module.

@ex[
(eval:alts (require "syntax/random.rkt") (void))
(random-expr)
(random-expr)
(random-expr)
(random-expr)
(for ([i (in-range 10)])
  (check-compiler (random-expr)))
]

This compiler has continues to have the issues identified in
@secref{broken}, but appears correct in its implementation of
conditional expressions.
