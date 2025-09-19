#lang scribble/manual

@(require (for-label (except-in racket compile ...) a86/ast a86/printer a86/registers a86/interp))
@(require scribble/examples
          redex/pict
	  "../fancyverb.rkt"
	  "../utils.rkt"
	  blackmail/semantics
	  "utils.rkt"
	  "ev.rkt")

@(define codeblock-include (make-codeblock-include #'here))

@(ev '(require rackunit a86))
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path langs "blackmail" f))))))
	   '("main.rkt" "random.rkt" "correct.rkt"))

@(define (shellbox . s)
   (parameterize ([current-directory (build-path langs "blackmail")])
     (filebox (emph "shell")
              (fancyverbatim "fish" (apply shell s)))))

@(require (for-syntax "../utils.rkt" racket/base "utils.rkt"))
@(define-syntax (shell-expand stx)
   (syntax-case stx ()
     [(_ s ...)
      (parameterize ([current-directory (build-path langs "blackmail")])
        (begin (apply shell (syntax->datum #'(s ...)))
	       #'(void)))]))

@;{ Have to compile 42.s (at expand time) before listing it }
@(shell-expand "cat add1-add1-40.rkt | racket -t compile-stdin.rkt -m > add1-add1-40.s")

@(define this-lang "Blackmail")
@(define prefix (string-append this-lang "-"))

@title[#:tag this-lang]{@|this-lang|: incrementing and decrementing}

@src-code[this-lang]

@emph{Let's Do It Again!}

@table-of-contents[]

@section{Refinement, take one}

We've seen all the essential pieces---a grammar, an AST data type
definition, a semantic specification (the interpreter), a compiler,
etc.---for implementing a programming language, albeit for an amazingly
simple language.

We will now, through a process of @bold{iterative refinement}, grow
the language to have an interesting set of features.


Our second language, which subsumes Abscond, is @bold{Blackmail}.
Expressions in Blackmail include integer literals and increment and
decrement operations.  It's still a dead simple language, but at least
programs @emph{do} something.

@section{Concrete syntax for Blackmail}

A Blackmail program consists of @tt{#lang racket} line and a
single expression, and the grammar of concrete expressions
is:

@centered{@render-language[B-concrete]}

So, @racket[0], @racket[120], and @racket[-42] are Blackmail expressions,
but so are @racket[(add1 0)], @racket[(sub1 120)], @racket[(add1
(add1 (add1 -42)))].

An example concrete program:

@codeblock-include["blackmail/add1-add1-40.rkt"]

@section{Abstract syntax for Blackmail}

The datatype for abstractly representing expressions can be defined
as:

@codeblock-include["blackmail/ast.rkt"]

So, @racket[(Lit 0)], @racket[(Lit 120)], and
@racket[(Lit -42)] are Blackmail AST expressions, but so are
@racket[(Prim1 'add1 (Lit 0))], @racket[(Sub1 (Lit 120))],
@racket[(Prim1 'add1 (Prim1 'add1 (Prim1 'add1 (Lit -42))))].


The parser is more involved than Abscond, but still
straightforward:

@codeblock-include["blackmail/parse.rkt"]

@ex[
(parse '42)
(parse '(add1 42))
(parse '(add1 (sub1 (add1 42))))]


@section{Meaning of Blackmail programs}

The meaning of a Blackmail program depends on the form of the expression:

@itemlist[
@item{the meaning of an integer literal is just the integer itself,}
@item{the meaning of an increment expression is one more than the meaning of its subexpression, and}
@item{the meaning of a decrement expression is one less than the meaning of its subexpression.}]

@;{
The operational semantics reflects this dependence on the form of the
expression by having three rules, one for each kind of expression:

@(define ((rewrite s) lws)
   (define lhs (list-ref lws 2))
   (define rhs (list-ref lws 3))
   (list "" lhs (string-append " " (symbol->string s) " ") rhs ""))

@(with-unquote-rewriter
   (lambda (lw)
     (build-lw (lw-e lw) (lw-line lw) (lw-line-span lw) (lw-column lw) (lw-column-span lw)))
   (with-compound-rewriters (['+ (rewrite '+)]
                             ['- (rewrite '–)])
     (centered (begin (judgment-form-cases '(0)) (render-judgment-form 𝑩))
               (hspace 4)
               (begin (judgment-form-cases '(1)) (render-judgment-form 𝑩))
	       (hspace 4)
	       (begin (judgment-form-cases '(2)) (render-judgment-form 𝑩)))))

The first rule looks familiar; it's exactly the semantics of integers
from Abscond.  The second and third rule are more involved.  In
particular, they have @bold{premises} above the line.  If the premises
are true, the @bold{conclusion} below the line is true as well.  These
rules are @emph{conditional} on the premises being true.  This is in
contrast to the first rule, which applies unconditionally.

We can understand these rules as saying the following:
@itemlist[
@item{For all integers @math{i}, @math{((Lit i),i)} is in @render-term[B 𝑩].}

@item{For expressions @math{e_0} and all integers @math{i_0} and
@math{i_1}, if @math{(e_0,i_0)} is in @render-term[B 𝑩] and @math{i_1
= i_0 + 1}, then @math{(@RACKET[(Prim1 'add1 (UNSYNTAX @math{e_0}))], i_1)}
is in @render-term[B 𝑩].}

@item{For expressions @math{e_0} and all integers @math{i_0} and
@math{i_1}, if @math{(e_0,i_0)} is in @render-term[B 𝑩] and @math{i_1
= i_0 - 1}, then @math{(@RACKET[(Prim1 'sub1 (UNSYNTAX @math{e_0}))], i_1)}
is in @render-term[B 𝑩].}
]

These rules are @bold{inductive}.  We start from the meaning of
integers and if we have the meaning of an expression, we can construct
the meaning of a larger expression.

This may seem a bit strange at the moment, but it helps to view the
semantics through its correspondence with an interpreter, which given
an expression @math{e}, computes an integer @math{i}, such that
@math{(e,i)} is in @render-term[B 𝑩].
}

To compute the meaning of an expression, the @racket[interp]
function does a case analysis of the expression:

@codeblock-include["blackmail/interp.rkt"]

In the case of a @racket[(Prim1 p e)] expression, the interpreter
first recursively computes the meaning of the subexpression @racket[e]
and then defers to a helper function @racket[interp-prim1] which
interprets the meaning of a given unary operation and value:

@codeblock-include["blackmail/interp-prim.rkt"]

If the given operation is @racket['add1], the function adds 1;
if it's @racket['sub1], it subtracts 1.

We can write examples of the @racket[interp] function, writing inputs
in abstract syntax:

@ex[
(interp (Lit 42))
(interp (Lit -7))
(interp (Prim1 'add1 (Lit 42)))
(interp (Prim1 'sub1 (Lit 8)))
(interp (Prim1 'add1 (Prim1 'add1 (Prim1 'add1 (Lit 8)))))
]

We could also write examples using concrete syntax, using the parser
to construct the appropriate abstract syntax for us:

@ex[
(interp (parse '42))
(interp (parse '-7))
(interp (parse '(add1 42)))
(interp (parse '(sub1 8)))
(interp (parse '(add1 (add1 (add1 8)))))
]



@;{
Here's how to connect the dots between the semantics and interpreter:
the interpreter is computing, for a given expression @math{e}, the
integer @math{i}, such that @math{(e,i)} is in @render-term[B 𝑩].  The
interpreter uses pattern matching to determine the form of the
expression, which determines which rule of the semantics applies.

@itemlist[

@item{if @math{e} is an integer @math{(Lit i)}, then we're done: this is the
right-hand-side of the pair @math{(e,i)} in @render-term[B 𝑩].}

@item{if @math{e} is an expression @RACKET[(Prim1 'add1 (UNSYNTAX
@math{e_0}))], then we recursively use the interpreter to compute
@math{i_0} such that @math{(e_0,i_0)} is in @render-term[B 𝑩].  But
now we can compute the right-hand-side by adding 1 to @math{i_0}.}

@item{if @math{e} is an expression @RACKET[(Prim1 'sub1 (UNSYNTAX
@math{e_0}))], then we recursively use the interpreter to compute
@math{i_0} such that @math{(e_0,i_0)} is in @render-term[B 𝑩].  But
now we can compute the right-hand-side by substracting 1 from @math{i_0}.}

]

This explanation of the correspondence is essentially a proof by
induction of the interpreter's correctness:

@bold{Interpreter Correctness}: @emph{For all Blackmail expressions
@racket[e] and integers @racket[i], if (@racket[e],@racket[i]) in
@render-term[B 𝑩], then @racket[(interp e)] equals
@racket[i].}

}

@section{An Example of Blackmail compilation}

Just as we did with Abscond, let's approach writing the compiler by
first writing an example.

Suppose we want to compile @racket[(add1 (add1 40))].  We already know
how to compile the @racket[40]: @racket[(Mov rax 40)].  To do the
increment (and decrement) we need to know a bit more a86.  In
particular, the @racket[Add] instruction is relevant.  It increments
the contents of a register by some given amount.

So, a program that adds 1 twice to 40 looks like:


@ex[
(asm-interp
  (prog (Global 'entry)
        (Label 'entry)
        (Mov rax 40)
        (Add rax 1)
        (Add rax 1)
        (Ret)))]
      

@;{filebox-include[fancy-nasm blackmail "add1-add1-40.s"]}


@section{A Compiler for Blackmail}

To compile Blackmail, we make use of two more a86 instructions,
@racket[Add] and @racket[Sub].  The compiler consists of two
functions: the first, which is given a program, emits the entry point
and return instructions, invoking another function to compile the
expression:

@codeblock-include["blackmail/compile.rkt"]

Notice that @racket[compile-e] is defined by structural
recursion, much like the interpreter.

In the case of a unary primitive @racket[(Prim1 p e)], the compiler
first compiles the subexpression @racket[e] obtaining a list of
instructions that, when executed, will place @racket[e]'s value in the
@racket[rax] register.  After that sequence of instructions, the
compiler emits instructions for carrying out the operation @racket[p],
defering to a helper function @racket[compile-op1]:

@codeblock-include["blackmail/compile-ops.rkt"]

This function either emits an @racket[Add] or @racket[Sub]
instruction, depending upon @racket[p].

We can now try out a few examples:

@ex[
(compile-e (parse '(add1 (add1 40))))
(compile-e (parse '(sub1 8)))
(compile-e (parse '(add1 (add1 (sub1 (add1 -8))))))]

To see the complete code for these examples, we can use the
@racket[compile] function:

@ex[
(compile (parse '(add1 (add1 40))))
(compile (parse '(sub1 8)))
(compile (parse '(add1 (add1 (sub1 (add1 -8))))))]

We can also run the code produced in these examples in order to see
what they each produce:

@ex[
(asm-interp (compile (parse '(add1 (add1 40)))))
(asm-interp (compile (parse '(sub1 8))))
(asm-interp (compile (parse '(add1 (add1 (sub1 (add1 -8)))))))]

Based on this, it's useful to define an @racket[exec] function that
(should) behave like @racket[interp], just as we did for Abscond:

@codeblock-include["blackmail/exec.rkt"]

@ex[
(exec (parse '(add1 (add1 40))))
(exec (parse '(sub1 8)))
(exec (parse '(add1 (add1 (sub1 (add1 -8))))))]

This function will be the basis of our compiler correctness statement
and a primary tool for testing the compiler.

And give a command line wrapper for parsing, checking, and compiling
in @link["code/blackmail/compile-stdin.rkt"]{@tt{compile-stdin.rkt}},
we can compile files as follows:

@shellbox["cat add1-add1-40.rkt | racket -t compile-stdin.rkt -m"]

And using the same @link["code/blackmail/Makefile"]{@tt{Makefile}}
setup as in Abscond, we capture the whole compilation process with a
single command:

@void[(shellbox "touch add1-add1-40.rkt")]
@shellbox["make add1-add1-40.run" "./add1-add1-40.run"]


@section{Correctness and random testing}

We can state correctness similarly to how it was stated for Abscond:

@bold{Compiler Correctness}: @emph{For all @racket[e] @math{∈}
@tt{Expr} and @racket[i] @math{∈} @tt{Integer}, if @racket[(interp e)]
equals @racket[i], then @racket[(exec e)] equals
@racket[i].}

(This statement is actually identical to the statement of correctness
for Abscond, however, it should be noted that the meaning of
@tt{Expr}, @racket[interp], @racket[exec] refer to their Blackmail
definitions.)

And we can test this claim by comparing the results of running
compiled and interpreted programs, leading to the following property,
which hopefully holds:

@codeblock-include["blackmail/correct.rkt"]

The problem, however, is that generating random Blackmail programs is
less obvious compared to generating random Abscond programs
(i.e. random integers).  Randomly generating programs for testing is
its own well studied and active research area.  To side-step this
wrinkle, we have provided a small utility for generating random
Blackmail programs (@link["code/blackmail/random.rkt"]{random.rkt}),
which you can use, without needing the understand how it was
implemented.

@ex[
(eval:alts (require "random.rkt") (void))
(random-expr)
(random-expr)
(random-expr)
(random-expr)
(random-expr)
(define e (random-expr))
e
(compile e)
(for ([i (in-range 10)])
  (check-compiler (random-expr)))
]

@section[#:tag "broken"]{A Broken Compiler}

It's now probably time to acknowledge a short-coming in our
compiler. Although it's great that random testing is
confirming the correctness of the compiler on
@emph{specific} examples, the compiler is unfortunately not
correct in general.  Neither was the Abscond compiler.

To see why, recall that integers in Blackmail are
represented as 64-bit values in the compiled code. The
problem arises when 64 bits isn't enough. Since the run-time
system interprets the 64-bit values as a @emph{signed}
integer, we have 1 bit devoted to the sign and 63 bits
devoted to the magnitude of the integer. So the largest
number we can represent is @racket[(sub1 (expt 2 63))] and
the smallest number is @racket[(- (expt 2 63))]. What
happens if a program exceeds these bounds? Well, whatever
x86 does.  Let's see:


@ex[
(define max-int (sub1 (expt 2 63)))
(define min-int (- (expt 2 63)))
(exec (Lit max-int))
(exec (Prim1 'add1 (Lit max-int)))
(exec (Lit min-int))
(exec (Prim1 'sub1 (Lit min-int)))]

Now there's a fact you didn't learn in grade school: in the
first example, adding 1 to a number made it smaller; in the
second, subtracting 1 made it bigger!

This problem doesn't exist in the interpreter:

@ex[
(interp (Lit max-int))
(interp (Prim1 'add1 (Lit max-int)))
(interp (Lit min-int))
(interp (Prim1 'sub1 (Lit min-int)))
]

So we have found a counter-example to the claim of compiler
correctness:

@ex[
(check-compiler (Prim1 'add1 (Lit max-int)))
(check-compiler (Prim1 'sub1 (Lit min-int)))
]

The problem also exists in Abscond in that we can write literals that
exceed these bounds.  The interpreter has no problem with such
literals:

@ex[
(interp (Lit (add1 max-int)))]

But the compiler will produce the wrong result:

@ex[
(exec (Lit (add1 max-int)))]

It's also possible to exceed the bounds so thoroughly, that the
program can't even be compiled:

@ex[
(interp (Lit (expt 2 64)))
(eval:error (exec (Lit (expt 2 64))))]

The issue here being that a @racket[Mov] instruction can only take an
argument that can be represented in 64-bits.


What can we do? This is the basic problem of a program not
satisfying its specification.  We have two choices:

@itemlist[
 @item{change the spec (i.e. the semantics and interpreter)}
 @item{change the program (i.e. the compiler)}
]

We could change the spec to make it match the behaviour of the
compiler.  This would involve writing out definitions that match the
``wrapping'' behavior we see in the compiled code. Of course if the
specification is meant to capture what Racket actually does, taking
this route would be a mistake. Even independent of Racket, this seems
like a questionable design choice. Wouldn't it be nice to reason about
programs using the usual laws of mathematics (or at least something as
close as possible to what we think of as math)? For example, wouldn't
you like know that @racket[(< i (add1 i))] for all integers
@racket[i]?

Unforunately, the other choice seems to paint us in to a
corner. How can we ever hope to represent all possible
integers in just 64 bits? We can't. We need some new tricks.
So in the meantime, our compiler is not correct, but writing
down what it means to be correct is half the battle. We will
get to correctness, but for the time being, we view the
specification aspirationally.


@section{Looking back, looking forward}

We've now built two compilers; enough to start observing a pattern.

Recall the phases of a compiler described in
@secref["What does a Compiler look like?"]. Let's identify
these pieces in the two compilers we've written:

@itemlist[
@item{@bold{Parsed} into a data structure called an @bold{Abstract Syntax Tree}

@itemlist[@item{we use @racket[read] to parse text into a s-expression}]

@itemlist[@item{we use @racket[parse] to convert an s-expression into an AST}]}

@item{@bold{Checked} to make sure code is well-formed

@itemlist[@item{we don't current do any, but more checking will come with more sophisticated langauges.}]}

@item{@bold{Optimized} into (equivalent) but faster program

@itemlist[@item{we don't do any yet}]}

@item{@bold{Generated} into assembly x86

@itemlist[@item{we use @racket[compile] to generate assembly (in AST form),
  and use @racket[asm-display] to print concrete X86-64 code}]}

@item{@bold{Linked} against a run-time (usually written in C)

@itemlist[@item{we link against our run-time written in @tt{main.c}}]}

]

Our recipe for building compiler involves:

@itemlist[#:style 'ordered
@item{Build intuition with @bold{examples},}
@item{Model problem with @bold{data types},}
@item{Implement compiler via @bold{type-guided-functions},}

@item{Validate compiler via @bold{tests}.}
]

As we move forward, the language we are compiling will grow.  As the
language grows, you should apply this recipe to grow the compiler
along with the language.
