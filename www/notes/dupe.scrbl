#lang scribble/manual

@(require (for-label (except-in racket ... compile) a86/printer a86/ast a86/registers a86/interp))
@(require redex/pict
          racket/runtime-path
          scribble/examples
          racket/format
          "../fancyverb.rkt"
	  "utils.rkt"
	  "ev.rkt"
	  dupe/semantics
          dupe/types
	  "../utils.rkt")



@(define codeblock-include (make-codeblock-include #'h))

@(ev '(require rackunit a86))
@(for-each (λ (f) (ev `(require (file ,(path->string (build-path langs "dupe" f))))))
	   '("main.rkt" "random.rkt" "correct.rkt"))


@title[#:tag "Dupe"]{Dupe: a duplicity of types}

@src-code["dupe"]

@emph{There are 10 types of values...}

@table-of-contents[]

@section{Integers and Booleans}

Up until now we have worked to maintain a single type of values:
integers.  This led to some awkwardness in the design of conditionals.
Let us lift this restriction by considering a multiplicity of types.
To start, we will consider two: integers and booleans.

We'll call it @bold{Dupe}.

We will use the following syntax, which relaxes the syntax of Con to
make @racket[(zero? _e)] its own expression form and conditionals can
now have arbitrary expressions in test position: @racket[(if _e0 _e1
_e2)] instead of just @racket[(if (zero? _e0) _e1 _e2)].  We also add
syntax for boolean literals.

Together this leads to the following grammar for concrete Dupe. 

@centered{@render-language[D-concrete]}

And abstract Dupe:

@codeblock-include["dupe/ast.rkt"]

The s-expression parser is defined as follows:

@codeblock-include["dupe/parse.rkt"]

@ex[
(parse '#t)
(parse '(if #t 1 2))
(parse '(zero? 8))
(parse '(if (zero? 8) 1 2))]

@section{Meaning of Dupe programs}

To consider the meaning of Dupe programs, we must revisit the meaning
of conditions.  Previously we branched on whether a subexpression
evaluated to 0.  We will now consider whether the subexpression
evaluates to @racket[#f].

Let's consider some examples:

@itemlist[

@item{@racket[(if #f 1 2)] means @racket[2] since the test expression means @racket[#f].}
@item{@racket[(if #t 1 2)] means @racket[1] since the test expression means @racket[#t], which is not @racket[#f].}

]

Just as in Racket (and Scheme, generally), we will treat any
non-@racket[#f] value as ``true.'' So:

@itemlist[

@item{@racket[(if 0 1 2)] means @racket[1] since the test expression means @racket[0], which is not @racket[#f].}
@item{@racket[(if 7 1 2)] means @racket[1] since the test expression means @racket[7], which is not @racket[#f].}

]

The new @racket[(zero? _e)] expression form evaluates to a boolean:
@racket[#t] when @racket[_e] evaluates to @racket[0] and @racket[#f]
to an integer other than @racket[0].

The @racket[#t] and @racket[#f] literals mean themselves, just like
integer literals.


Once a multiplicity of types are introduced, we are forced to come to
grips with @bold{type mismatches}.  For example, what should
@racket[(add1 #f)] mean?

Languages adopt several approaches:

@itemlist[

@item{Prohibit such programs statically with a type system (e.g. OCaml, Java)}
@item{Coerce values to different types (e.g. JavaScript)}
@item{Signal a run-time error (e.g. Racket)}
@item{Leave the behavior unspecified (e.g. Scheme, C)}

]

We are going to start by taking the last approach.  Later we can
reconsider the design, but for now this is a simple approach.

@;{
The semantics is still a binary relation between expressions and their
meaning, however the type of the relation is changed to reflect the
values of Dupe, which may either be integers or booleans:


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

@(show-judgment 𝑫 0 3)
@(show-judgment 𝑫 3 5)
@(show-judgment 𝑫 5 7)

This relies on two helper judgments:

@(show-judgment is-true 0 2)
@(show-judgment is-false 0 1)

Notice that here we are following the semantics of Racket, which says
that anything that is not @racket[#f] counts as @emph{true} in a
conditional.

This semantics has also been refactored slightly so that
there is a single rule for the @racket[Prim1] case. The new
rule essentially defers the work to a new metafunction,
@math{𝑫-𝒑𝒓𝒊𝒎}:

@(centered
  (with-compound-rewriters (['+ (rewrite '+)]
                            ['- (rewrite '–)]
                            ['= (rewrite '=)]
                            ['!= (rewrite '≠)])
    (render-metafunction 𝑫-𝒑𝒓𝒊𝒎 #:contract? #t)))

Returning to the issue of type mismatches, what does the
semantics say about @racket[(Prim1 'add1 (Lit #f))]?

What it says is: nothing.  These programs are simply not in the
semantic relation for this language. There's only one rule for giving
meaning to an @racket[(Prim1 'add1 _e0)] expression and it's premise is that
@racket[_e] means some @emph{integer} @racket[_i0].  But
@math{(@racket[(Lit #f)], i) ∉ 𝑫} for any @math{i}.  So there's no value
@math{v} such that @math{(@racket[(Prim1 'add1 (Lit #f))], v) ∈ 𝑫}.  This
expression is @bold{undefined} according to the semantics.
}

The interpreter follows a similar pattern to what we've done so far,
although notice that the result of interpretation is now a @tt{Value}:
either a boolean or an integer:

@codeblock-include["dupe/interp.rkt"]

The interpretation of primitives is extended to account for the new
@racket[zero?] primitive:

@codeblock-include["dupe/interp-prim.rkt"]


We can confirm the interpreter computes the right result for the
examples given earlier:

@ex[
(interp (parse #t))
(interp (parse #f))
(interp (parse '(if #f 1 2)))
(interp (parse '(if #t 1 2)))
(interp (parse '(if 0 1 2)))
(interp (parse '(if 7 1 2)))
(interp (parse '(if (zero? 7) 1 2)))]

@section{(Lack of) Meaning for some Dupe programs}

Viewed as a specification, what is this interpreter saying about programs that
do nonsensical things like @racket[(add1 #f)]?

First, let's revise the statement of compiler correctness to reflect
the fact that @racket[interp] can return different kinds of values:

@bold{Compiler Correctness}: @emph{For all @racket[e] @math{∈}
@tt{Expr} and @racket[v] @math{∈} @tt{Value}, if @racket[(interp e)]
equals @racket[v], then @racket[(exec e)] equals
@racket[v].}

Now, the thing to notice here is that this specification only
obligates the compiler to produce a result consistent with the
interpreter when the interpreter produces a @emph{value}.  It says
nothing about what the compiler must produce if the intepreter fails
to produce a value, which is exactly what happens when the interpreter
is run on examples like @racket[(add1 #f)]:

@ex[
(eval:error (interp (parse '(add1 #f))))]

Since the intepreter does not return a value on such an input, the
meaning of such expression is undefined and the compiler is
unconstrained and may do whatever it likes: crash at compile-time,
crash at run-time, return 7, format your hard drive; there are no
wrong answers.  (These possibilities hopeful make clear while
undefined behavior opens up all kinds of bad outcomes and has been the
source of many costly computing failures and vulnerabilities and why
as a language designer, leaving the behavior of some programs
undefined may not be a great choice.  As a compiler implementor,
however, it makes our life easier: we simply don't need to worry about
what happens on these kinds of programs.)

This will, however complicate testing the correctness of the compiler,
which is addressed in @secref["Correctness_and_testing"].

Let's now turn to the main technical challenge introduced by the Dupe
langauge.

@section{Ex uno plures: Out of One, Many}

Before getting in to the compiler, let's first address the issue of
representation of values in the setting of x86.

So far we've had a single type of value: integers.  Luckily, x86 has a
convenient datatype for representing integers: it has 64-bit signed
integers.  (There is of course the problem that this representation is
@bold{inadequate}; there are many (Con) integers we cannot represent
as a 64-bit integer.  But we put off worrying about this until later.)

The problem now is how to represent integers @emph{and} booleans,
which should be @bold{disjoint} sets of values.  Representing these
things in the interpreter as Racket values was easy: we Racket
booleans to represent Dupe booleans; we used Racket integers to
represent Dupe integers.  Since Racket booleans and integers are
disjoint types, everything was easy and sensible.

Representing this things in x86 will be more complicated.  x86 doesn't
have a notion of ``boolean'' per se and it doesn't have a notion of
``disjoint'' datatypes.  There is only one data type and that is:
bits.

To make the problem concrete, consider the Dupe expression
@racket[5]; we know the compiler is going to emit an single instruction
that moves this value into the @racket[rax] register:

@ex[
(Mov rax 5)]

But now consider @racket[#t].  The compiler needs to emit an
instruction that moves ``@racket[#t]'' into @racket[rax], but the
@racket[Mov] instruction doesn't take booleans:

@ex[
(eval:error (Mov rax #t))]

We have to move some 64-bit integer into @racket[rax], but the
question is: which one?

The immediate temptation is to just pick a couple of integers, one for
representing @racket[#t] and one for @racket[#f].  We could follow the
C tradition and say @racket[#f] will be @racket[0] and @racket[#t]
will be 1.  So compiling @racket[#t] would emit:

@ex[
(Mov rax 1)]

And compiling @racket[#f] would emit:

@ex[
(Mov rax 0)]

Seems reasonable.  Well except that the specification of @racket[if]
in our interpreter requires that @racket[(if 0 1 2)] evaluates to
@racket[1] and @racket[(if #f 1 2)] evaluates to @racket[2].  But
notice that @racket[0] and @racket[#f] compile to exactly the same
thing under the above scheme.  How could the code emitted for
@racket[if] possibly distinguish between whether the test expression
produced @racket[#f] or @racket[0] if they are represented by the same
bits?

So the compiler is doomed to be incorrect on some expressions under
this scheme.  Maybe we should revise our choice and say @racket[#t]
will be represented as @racket[0] and @racket[#f] should be
@racket[1], but now the compiler must be incorrect either on
@racket[(if #f 1 2)] or @racket[(if 1 1 2)].  We could choose
different integers for the booleans, but there's no escaping it: just
picking some bits for @racket[#t] and @racket[#f] without also
changing the representation of integers is bound to fail.

Here's another perspective on the same problem.  Our compiler emits
code that leaves the value of an expression in the @racket[rax]
register, which is how the value is returned to the run-time system
that then prints the result.  Things were easy before having only
integers: the run-time just prints the integer.  But with booleans,
the run-time will need to print either @tt{#t} or @tt{#f} if the
result is the true or false value.  But if we pick integers to
represent @racket[#t], how can the run-time know whether it should
print the result as an integer or as @tt{#t}?  It can't!

The fundamental problem here is that according to our specification,
@racket[interp], these values need to be disjoint.  No value can be
both an integer @emph{and} a boolean.  Yet in x86 world, only one kind
of thing exists: bits, so how can we make values of different kinds?

Ultimately, the only solution is to incorporate an explicit
representation of the @emph{type} of a value and use this information
to distinguish between values of different type.  We could do this in
a number of ways: we could desginate another register to hold (a
representation of) the type of the value in @racket[rax].  This would
mean you couldn't know what the value in @racket[rax] meant without
consulting this auxiliary register.  In essence, this is just using
more than 64-bits to represent values.  Alternatively, we could
instead encode this information within the 64-bits so that only a
single register is needed to completely determine a value.  This does
come at a cost though: if some bits are needed to indicate the type,
there are fewer bits for the values!

Here is the idea of how this could be done: We have two kinds of data:
integers and booleans, so we could use one bit to indicate whether a
value is a boolean or an integer.  The remaining 63 bits can be used
to represent the value itself, either true, false, or some integer.
There are other approaches to solving this problem, but this is a
common approach called @emph{type-tagging}.

Let's use the least significant bit to indicate the type and
let's use @binary[type-int] for integer and
@binary[(value->bits #t)] for boolean. These are arbitrary choices
(more or less).

The number @racket[1] would be represented as
@binary[(value->bits 1)], which is the bits for the number
(i.e. @binary[1]), followed by the integer tag
(@binary[type-int]). Note that the representation of a Dupe
number is no longer the number itself: the Dupe value
@racket[1] is represented by the number @racket[2]
(@binary[2]). The Dupe value @racket[#t]
is represented by the number @racket[#,(value->bits #t)]
(@binary[(value->bits #t) 2]); the Dupe value @racket[#f]
is represented by the number @racket[#,(value->bits #f)]
(@binary[(value->bits #f) 2]).

One nice thing about our choice of encoding: @racket[0] is represented
as @racket[0] (@binary[0 2]).


To encode a value as bits:

@itemlist[

@item{If the value is an integer, shift the value to the left one bit. (Mathematically, this has the effect of doubling the number.)}
@item{If the value is a boolean,
  @itemlist[
    @item{if it's the boolean @racket[#t], encode as @racket[#,(value->bits #t)],}
    @item{if it's the value @racket[#f], encode as @racket[#,(value->bits #f)].}]}]

To decode bits as a value:

@itemlist[
@item{If the least significant bit is @racket[0], shift to the right one bit.  (Mathematically, this has the effect of halving the number.)}
@item{If the bits are @racket[#,(value->bits #t)], decode to @racket[#t].}
@item{If the bits are @racket[#,(value->bits #f)], decode to @racket[#f].}
@item{All other bits don't encode a value.}]


If you wanted to determine if a 64-bit integer represented
an integer value or a boolean value, you simply need to inquire about
the value of the least significant bit. Mathematically,
this just corresponds to asking if the number is even or
odd. Odd numbers end in the bit (@binary[1]), so they
reprepresent booleans. Even numbers represent integers. Here
are some functions to check our understanding of the
encoding:

@codeblock-include["dupe/types.rkt"]

@#reader scribble/comment-reader
(ex
(bits->value #b000)
(bits->value #b001)
(bits->value #b010)
(bits->value #b011)
(bits->value #b100)
(eval:error (bits->value #b101))
(bits->value #b110)
(eval:error (bits->value #b111))
)

Notice that not all bits represent a value; name any odd number that's
neither 1 (@racket[#f]) or 3 (@racket[#t]).

We can also write the inverse:

@#reader scribble/comment-reader
(ex

(value->bits #t)
(value->bits #f)
(value->bits 0)
(value->bits 1)
(value->bits 2)
(value->bits 3)


(bits->value (value->bits #t))
(bits->value (value->bits #f))
(bits->value (value->bits 0))
(bits->value (value->bits 1))
(bits->value (value->bits 2))
(bits->value (value->bits 3))

)

The interpreter operates at the level of @tt{Value}s.  The compiler
will have to work at the level of @tt{Bits}.


@;{
Of course, we could,
as an intermediate step, define an interpreter that works on bits,
which may help us think about how to implement the compiler.

We want a function
@#reader scribble/comment-reader
(racketblock
;; interp-bits : Expr -> Bits
)
such that
@#reader scribble/comment-reader
(racketblock
;; ∀ e : Expr . (interp-bits e) = (value->bits (interp e))
)

Let's design @racket[interp-bits] by derivation.  This is a common
functional programming technique whereby we start from a specification
program and through a series of algebraic transformations, arrive an
correct implementation.

First, let's state the specification of @racket[interp-bits] as a
function that ``cheats,'' it uses @racket[interp] to carry out
evaluation and then finally converts to bits.

@#reader scribble/comment-reader
(ex
;; Expr -> Bits
(define (interp-bits e)
  (value->bits (interp e)))
)

It's clearly correct with respect to the spec for @racket[interp-bits]
that we started with because @emph{the code is just the specification
itself}.

We can even do some property-based random testing (which obviously
succeeds):

@ex[#:no-prompt
(define (interp-bits-correct e)
  (with-handlers ([exn:fail? (λ (x) 'ok)])
    (interp e)
    (check-equal? (interp-bits e)
                  (value->bits (interp e))
		  (format "~a" e))))

(define es
  (for/list ([i 100])
    (random-expr)))

(for-each interp-bits-correct es)
]

The one wrinkle is we really only need the spec to hold when
@racket[_e] is defined according to the semantics.  Since we know
@racket[interp] crashes (by raising an exception) whenever @racket[_e]
is undefined, we use an exception handler to avoid testing when
@racket[_e] is undefined.

Now let us inline the defintion of @racket[interp], i.e. let's replace
the use of @racket[interp] with it's definition:

@#reader scribble/comment-reader
(ex #:no-prompt
;; Expr -> Bits
(define (interp-bits e)
  (value->bits
   (match e
     [(Lit l) l]
     [(Prim1 p e)
      (interp-prim1 p (interp e))]
     [(If e1 e2 e3)
      (if (interp e1)
          (interp e2)
          (interp e3))])))
)

It's still correct:
@ex[
(for-each interp-bits-correct es)]

Now let's ``push'' the @racket[value->bits] inward by using the following equation:
@racketblock[
(_f (match _m [_p _e] ...)) = (match _m [_p (_f _e)] ...)
]

So we get:

@#reader scribble/comment-reader
(ex #:no-prompt
;; Expr -> Bits
(define (interp-bits e)
  (match e
    [(Lit l) (value->bits l)]
    [(Prim1 p e)
     (value->bits
      (interp-prim1 p (interp e)))]
    [(If e1 e2 e3)
     (value->bits
      (if (interp e1)
          (interp e2)
          (interp e3)))])))


Still correct:
@ex[
(for-each interp-bits-correct es)]

Now consider the first case, where we are calling @racket[value->bits]
on @racket[i], which we know is an integer whenever this clause of the
@racket[match] is taken.  From looking at the definition of
@racket[value->bits], we know @racket[(value->bits i)] is
@racket[(arithmetic-shift i int-shift)] when @racket[i] is an integer.
So we can replace the RHS of the first case with
@racket[(arithmetic-shift i int-shift)].

We can do similar reasoning on the second case with
@racket[(value->bits b)] where @racket[b] is a boolean.  From the
definition of @racket[value->bits], we can replace the RHS with
@racket[(match b [#t (value->bits #t)] [#f (value->bits #f)])], which can be written
more succinctly as @racket[(if b (value->bits #t) (value->bits #f))].

In the third case, let's suppose there is an analog of
@racket[interp-prim1] called @racket[interp-prim1-bits] that operates
on, and produces, bits.  As a start, we can assume a definition that
is just the specification of this function (we'll derive a better
version later):

@#reader scribble/comment-reader
(ex #:no-prompt
;; Op Bits -> Bits
(define (interp-prim1-bits p b)
  (value->bits (interp-prim1 p (bits->value b))))
)

Now we can replace the RHS of the third case with
@racket[(interp-prim1-bits p (interp-bits e))].

Finally, in the fourth case, we can use the following equality:

@racketblock[
(_f (if _e0 _e1 _e2)) = (if _e0 (_f _e1) (_f _e2))
]

to arrive at:

@racketblock[
(if (interp e0)
    (value->bits (interp e1))
    (value->bits (interp e2)))
]

Of course, @racket[(value->bits (interp e1))] is just what
@racket[interp-bits] computes, so this is equivalent to:

@racketblock[
(if (interp e0)
    (interp-bits e1)
    (interp-bits e2))
]

Now observe that @racket[(interp e0)] produces @racket[#f] if and only
if @racket[(interp-bits e0)] produces @racket[(value->bits #f)].  We can therefore
eliminate the use of @racket[interp] by replacing this conditional with:

@racketblock[
(if (eq? (value->bits #f) (interp-bits e0))
    (interp-bits e2)
    (interp-bits e1))
]

(Notice the swapping of the then- and else-branch of the conditional.)

We've now arrived at the following @racket[interp]-free definition of
@racket[interp-bits]:

@#reader scribble/comment-reader
(ex #:no-prompt
;; Expr -> Bits
(define (interp-bits e)
  (match e
    [(Lit l) (value->bits l)]
    [(Prim1 p e)
     (interp-prim1-bits p (interp-bits e))]
    [(If e1 e2 e3)
     (if (eq? (value->bits #f) (interp-bits e1))
         (interp-bits e3)
         (interp-bits e2))])))

And it is still correct:
@ex[
(for-each interp-bits-correct es)]


We're almost done.  Now let's derive a version of
@racket[interp-prim1-bits] starting from the specification we gave
above.  To start, replace the use of @racket[interp-prim1] with its
definition:

@#reader scribble/comment-reader
(ex #:no-prompt
;; Op Bits -> Bits
(define (interp-prim1-bits op b)
  (value->bits
   (match op
     ['add1  (add1 (bits->value b))]
     ['sub1  (sub1 (bits->value b))]
     ['zero? (zero? (bits->value b))]))))

Now push @racket[value->bits] inward:

@#reader scribble/comment-reader
(ex #:no-prompt
;; Op Bits -> Bits
(define (interp-prim1-bits op b)
  (match op
    ['add1  (value->bits (add1 (bits->value b)))]
    ['sub1  (value->bits (sub1 (bits->value b)))]
    ['zero? (value->bits (zero? (bits->value b)))])))

Now notice the following:

@itemlist[

@item{@racket[(value->bits (add1 (bits->value b)))] ≡ @racket[(+ b (value->bits 1))] ≡ @racket[(+ b (arithmetic-shift 1 int-shift))]}

@item{@racket[(value->bits (sub1 (bits->value b)))] ≡ @racket[(- b (value->bits 1))] ≡ @racket[(- b (arithmetic-shift 1 int-shift))]}

@item{@racket[(value->bits (zero? (bits->value b)))] ≡ @racket[(value->bits (zero? b))] ≡ @racket[(if (zero? b) (value->bits #t) (value->bits #f))]}

]

So we can define @racket[interp-prim1-bits] as:

@#reader scribble/comment-reader
(ex #:no-prompt
;; Op Bits -> Bits
(define (interp-prim1-bits op b)
  (match op
    ['add1  (+ b (arithmetic-shift 1 int-shift))]
    ['sub1  (- b (arithmetic-shift 1 int-shift))]
    ['zero? (if (zero? b) (value->bits #t) (value->bits #f))])))


@;{


In the first two cases, we know that @racket[i] and @racket[b] are
integers and booleans, respectively.  So we know @racket[(values->bits
i) = (* 2 i)] and @racket[(values->bits b) = (if b #,(value->bits #t) #,(value->bits #f))].  We can
rewrite the code as:

@;{the #:escape identity thing is a cute solution to the
 problem of escaping within examples.}

@ex[#:escape identity #:no-prompt
@code:comment{Expr -> Bits}
 (define (interp-bits e)
   (match e
     [(Lit l) (value->bits l)]
     [(Prim1 'add1 e0)
      (value->bits (add1 (interp e0)))]
     [(Prim1 'sub1 e0)
      (value->bits (sub1 (interp e0)))]
     [(Prim1 'zero? e0)
      (value->bits (zero? (interp e0)))]
     [(If e0 e1 e2)
      (value->bits
       (if (interp e0)
           (interp e1)
           (interp e2)))]))
]

Still correct:
@ex[
(for-each interp-bits-correct es)]


We can rewrite the last case by the following equation:
@racketblock[
(_f (if _e0 _e1 _e2)) = (if _e0 (_f _e1) (_f _e2))
]

@ex[#:escape identity #:no-prompt
@code:comment{Expr -> Bits}
 (define (interp-bits e)
   (match e
     [(Lit i) (* 2 i)]
     [(Lit b) (if b (identity (value->bits #t)) (identity (value->bits #f)))]
     [(Prim1 'add1 e0)
      (value->bits (add1 (interp e0)))]
     [(Prim1 'sub1 e0)
      (value->bits (sub1 (interp e0)))]
     [(Prim1 'zero? e0)
      (value->bits (zero? (interp e0)))]
     [(If e0 e1 e2)      
      (if (interp e0)
          (value->bits (interp e1))
          (value->bits (interp e2)))]))
]

Still correct:
@ex[
(for-each interp-bits-correct es)]


Let's now re-write by the following equations:

@racketblock[
(add1 _e) = (+ _e 1)
(sub1 _e) = (- _e 1)
(f (+ _e0 _e1)) = (+ (f _e0) (f _e1))
(f (- _e0 _e1)) = (- (f _e0) (f _e1))
]

to get:

@ex[#:escape identity #:no-prompt
@code:comment{Expr -> Bits}
 (define (interp-bits e)
   (match e
     [(Lit i) (* 2 i)]
     [(Lit b) (if b (identity (value->bits #t)) (identity (value->bits #f)))]
     [(Prim1 'add1 e0)
      (+ (value->bits (interp e0)) (value->bits 1))]
     [(Prim1 'sub1 e0)
      (- (value->bits (interp e0)) (value->bits 1))]
     [(Prim1 'zero? e0)
      (value->bits (zero? (interp e0)))]
     [(If e0 e1 e2)      
      (if (interp e0)
          (value->bits (interp e1))
          (value->bits (interp e2)))]))
]

Still correct:
@ex[
(for-each interp-bits-correct es)]

By computation, @racket[(value->bits 1) = #,(value->bits 1)].

We can now rewrite by the equation of our specification:

@racketblock[
(value->bits (interp _e)) = (interp-bits _e)]


@ex[#:escape identity #:no-prompt
@code:comment{Expr -> Bits}
 (define (interp-bits e)
   (match e
     [(Lit i) (* 2 i)]
     [(Lit b) (if b (identity (value->bits #t)) (identity (value->bits #f)))]
     [(Prim1 'add1 e0)
      (+ (interp-bits e0) (identity (value->bits 1)))]
     [(Prim1 'sub1 e0)
      (- (interp-bits e0) (identity (value->bits 1)))]
     [(Prim1 'zero? e0)
      (value->bits (zero? (interp e0)))]
     [(If e0 e1 e2)      
      (if (interp e0)
          (interp-bits e1)
          (interp-bits e2))]))
]

Still correct:
@ex[
(for-each interp-bits-correct es)]

We can rewrite by the equation
@racketblock[(zero? (interp _e0)) = (zero? (interp-bits _e0))]
and inline @racket[value->bits] specialized to a boolean argument:

@ex[#:escape identity #:no-prompt
@code:comment{Expr -> Bits}
 (define (interp-bits e)
   (match e
     [(Lit i) (* 2 i)]
     [(Lit b) (if b (identity (value->bits #t)) (identity (value->bits #f)))]
     [(Prim1 'add1 e0)
      (+ (interp-bits e0) (identity (value->bits 1)))]
     [(Prim1 'sub1 e0)
      (- (interp-bits e0) (identity (value->bits 1)))]
     [(Prim1 'zero? e0)
      (match (zero? (interp-bits e0))
        [#t (identity (value->bits #t))]
        [#f (identity (value->bits #f))])]
     [(If e0 e1 e2)
      (if (interp e0)
          (interp-bits e1)
          (interp-bits e2))]))
 ]

Still correct:
@ex[
(for-each interp-bits-correct es)]

Finally, in the last case, all that matters in @racket[(if (interp e0)
...)] is whether @racket[(interp e0)] returns @racket[#f] or something
else.  So we can rewrite in terms of whether @racket[(interp-bits e0)]
produces the representation of @racket[#f] (@binary[(value->bits #f) 2]):

@ex[#:escape identity #:no-prompt
@code:comment{Expr -> Bits}
 (define (interp-bits e)
   (match e
     [(Lit l)
      (cond
        [(integer? l) (* 2 l)]
        [(boolean? l)
         (if l (identity (value->bits #t)) (identity (value->bits #f)))])]
     [(Prim1 'add1 e0)
      (+ (interp-bits e0) (identity (value->bits 1)))]
     [(Prim1 'sub1 e0)
      (- (interp-bits e0) (identity (value->bits 1)))]
     [(Prim1 'zero? e0)
      (match (zero? (interp-bits e0))
        [#t (identity (value->bits #t))]
        [#f (identity (value->bits #f))])]
     [(If e0 e1 e2)
      (if (= (interp-bits e0) (identity (value->bits #f)))
          (interp-bits e2)
          (interp-bits e1))]))
 ]


Still correct:
@ex[
(for-each interp-bits-correct es)]


Note that whenever @racket[_bs] are bits representing an integer, then
@racket[(value->bits (add1 (bits->value _bs)))] is equal to @racket[(+
_bs #,(value->bits 1))], i.e. adding @binary[(value->bits 1) 2].  When @racket[_bs]
represents a boolean, then @racket[(value->bits (add1 (bits->value
_bs)))] would crash, while @racket[(+ _bs (value->bits 1))] doesn't, but this is an
undefined program, so changing the behavior is fine.
}


Looking back: starting from the spec, we've arrived at a definition of
@racket[interp-bits] that is completely self-contained: it doesn't use
@racket[interp] at all.  It also only uses the @tt{Bits}
representation and does no conversions to or from @tt{Value}s.

We can recover the original interpreter by wrapping the bit
interpreter in a final conversion:

@#reader scribble/comment-reader
(ex
;; Expr -> Value
(define (interp.v2 e)
  (bits->value (interp-bits e)))

(interp.v2 (Lit #t))
(interp.v2 (Lit #f))
(interp.v2 (parse '(if #f 1 2)))
(interp.v2 (parse '(if #t 1 2)))
(interp.v2 (parse '(if 0 1 2)))
(interp.v2 (parse '(if 7 1 2)))
(interp.v2 (parse '(if (zero? 7) 1 2)))
(eval:error (interp.v2 (parse '(add1 #f))))
(interp.v2 (parse '(add1 #t)))
)

Notice the last two examples.  What's going on?

The @racket[interp.v2] function is also a correct interpreter for
Dupe, and importantly, it sheds light on how to implement the compiler
since it uses the same representation of values.
}

@section{An Example of Dupe compilation}

The most significant change from Con to Dupe for the compiler is the
change in representation of values.

Let's consider some simple examples:

@itemlist[

@item{@racket[42]: this should compile just like integer literals
before, but needs to use the new representation, i.e. the compiler
should produce @racket[(Mov rax #,(value->bits 42))], which is
@racket[42] shifted to the left @racket[#,int-shift]-bit.}

@item{@racket[#f]: this should produce @racket[(Mov rax #,(value->bits #f))].}

@item{@racket[#t]: this should produce @racket[(Mov rax #,(value->bits #t))].}

@item{@racket[(add1 _e)]: this should produce the instructions for
@racket[_e], which when executed would leave @emph{the encoding of the
value of @racket[_e]} in the @racket[rax] register.  To these
instructions, the compiler needs to append instructions that will
leave the encoding of one more than the value of @racket[_e] in
@racket[rax].  In other words, it should add @racket[#,(value->bits
1)] to @racket[rax]!}

@item{@racket[(sub1 _e)]: should work like @racket[(add1 _e)] but
subtracting @racket[#,(value->bits 1)].}

@item{@racket[(zero? _e)]: this should produce the instructions for
  @racket[_e] followed by instructions that compare @racket[rax] to
  the encoding of the value @racket[0], which is just the bits
  @racket[0], and set @racket[rax] to @racket[#t]
  (i.e. @binary[(value->bits #t) 2]) if true and @racket[#f]
  (i.e. @binary[(value->bits #f) 2]) otherwise.

This is a bit different from what we saw with Con, which combined
conditional execution with testing for equality to @racket[0].  Here
there is no need to @emph{jump} anywhere based on whether @racket[_e]
produces @racket[0] or not.  Instead we want to move either the
encoding of @racket[#t] or @racket[#f] into @racket[rax] depending on
what @racket[_e] produces.  To accomplish that, we can use a new kind
of instruction, the @bold{conditional move} instruction: @racket[Cmov].

}

@item{@racket[(if _e0 _e1 _e2)]: this should work much like before,
compiling each subexpression, generating some labels and the
appropriate comparison and conditional jump.  The only difference is
we now want to compare the result of executing @racket[_e0] with
(the encoding of the value) @racket[#f] (i.e. @binary[(value->bits #f) 2]) and jumping to the code for @racket[_e2] when
they are equal.}
]

@ex[
(compile-e (Lit 42))
(compile-e (Lit #t))
(compile-e (Lit #f))
(compile-e (parse '(zero? 0)))
(compile-e (parse '(if #t 1 2)))
(compile-e (parse '(if #f 1 2)))
]


@section{A Compiler for Dupe}

Based on the examples, we can write the compiler.  Notice that the
compiler uses the @racket[value->bits] function we wrote earlier for
encoding values as bits.  This helps make the code more readable and
easier to maintain should the encoding change in the future.  But it's
important to note that this function is used only at compile-time.  By
the time the assemble code executes (i.e. run-time) the
@racket[value->bits] function (and indeed all Racket functions) no
longer exists.

@codeblock-include["dupe/compile.rkt"]

The compilation of primitives, including the new @racket[zero?]
primitive, can be accomplished with:

@codeblock-include["dupe/compile-ops.rkt"]

We can try out the compiler with the help of @racket[asm-interp],
but you'll notice the results are a bit surprising:

@ex[
(asm-interp (compile (Lit #t)))
(asm-interp (compile (Lit #f)))
(asm-interp (compile (parse '(zero? 0))))
(asm-interp (compile (parse '(zero? -7))))
(asm-interp (compile (parse '(if #t 1 2))))
(asm-interp (compile (parse '(if #f 1 2))))
(asm-interp (compile (parse '(if (zero? 0) (if (zero? 0) 8 9) 2))))
(asm-interp (compile (parse '(if (zero? (if (zero? 2) 1 0)) 4 5))))
]

The reason for this is @racket[asm-interp] doesn't do any
interpretation of the bits it gets back; it is simply
producing the integer that lives in @racket[rax] when the
assembly code finishes. This suggests adding a call to
@racket[bits->value] can be added to interpret the bits as
values:

@ex[
(bits->value (asm-interp (compile (Lit #t))))]

Which leads us to the following definition of @racket[exec]:

@codeblock-include["dupe/exec.rkt"]

@ex[
(exec (parse #t))
(exec (parse #f))
(exec (parse '(zero? 0)))
(exec (parse '(zero? -7)))
(exec (parse '(if #t 1 2)))
(exec (parse '(if #f 1 2)))
(exec (parse '(if (zero? 0) (if (zero? 0) 8 9) 2)))
(exec (parse '(if (zero? (if (zero? 2) 1 0)) 4 5)))]

The one last peice of the puzzle is updating the run-time system to
incorporate the new representation.  The run-time system is
essentially playing the role of @racket[bits->value]: it determines
what is being represented and prints it appropriately.

@section{Updated Run-time System for Dupe}

Any time there's a change in the representation or set of values,
there's going to be a required change in the run-time system.  From
Abscond through Con, there were no such changes, but now we have to
udpate our run-time system to reflect the changes made to values in
Dupe.

We define the bit representations in a header file corresponding to
the definitions given in @tt{types.rkt}:

@filebox-include[fancy-c dupe "types.h"]

It uses an idiom of ``masking'' in order to examine on
particular bits of a value. So for example if we want to
know if the returned value is an integer, we do a
bitwise-and of the result and @tt{1}. This produces a single
bit: 0 for integer and 1 for boolean. In the case of an
integer, to recover the number being represented, we need to
divide by 2, which can be done efficiently with a
right-shift of 1 bit. Likewise with a boolean, if we shift
right by 1 bit there are two possible results:
@racket[#,(value->bits #f)] for false and @racket[#,(value->bits #t)] for
true.

We use the following interface for values in the runtime system:

@filebox-include[fancy-c dupe "values.h"]
@filebox-include[fancy-c dupe "values.c"]

The @tt{main} function remains largely the same although now we use
@tt{val_t} in place of @tt{int64_t}:

@filebox-include[fancy-c dupe "main.c"]

And finally, @tt{print_result} is updated to do a case analysis on the
type of the result and print accordingly:

@filebox-include[fancy-c dupe "print.c"]

@section{Correctness and testing}

We already established our definition of correctness:

@bold{Compiler Correctness}: @emph{For all @racket[e] @math{∈}
@tt{Expr} and @racket[v] @math{∈} @tt{Value}, if @racket[(interp e)]
equals @racket[v], then @racket[(exec e)] equals
@racket[v].}

As a starting point for testing, we can consider a revised version of
@racket[check-compiler] that also uses @racket[bits->value]:

@ex[
(define (check-compiler* e)
  (check-equal? (interp e)
                (exec e)))]

This works just fine for meaningful expressions:

@ex[
(check-compiler* (parse #t))
(check-compiler* (parse '(add1 5)))
(check-compiler* (parse '(zero? 4)))
(check-compiler* (parse '(if (zero? 0) 1 2)))]

But, as discussed earlier, the hypothetical form of the compiler
correctness statement is playing an important role: @emph{if} the
interpreter produces a value, the compiler must produce code that when
run produces the (representation of) that value.  But if the
interpeter does not produce a value, all bets are off.

From a testing perspective, this complicates how we use the
interpreter to test the compiler. If every expression has a meaning
according to @racket[interp] (as in all of our previous languages), it
follows that the interpreter cannot crash on any @tt{Expr} input.
That makes testing easy: think of an expression, run the interpreter
to compute its meaning and compare it to what running the compiled
code produces.  But now the interpreter may break if the expression is
undefined.  We can only safely run the interpreter on expressions that
have a meaning.

This means the above definition of @racket[check-compiler] won't
suffice, because this function crashes on inputs like @racket[(add1
#f)], even though such an example doesn't actually demonstrate a
problem with the compiler:

@ex[
(check-compiler* (parse '(add1 #f)))]

To overcome this issue, we can take advantage of Racket's exception
handling mechanism to refine the @racket[check-compiler] function:

@codeblock-include["dupe/correct.rkt"]

This version installs an exception handler around the call to
@racket[interp] and in case the interpreter raises an exception (such
as when happens when interpreting @racket[(add1 #f)]), then the
handler simply returns the exception itself.

The function then guards the @racket[check-equal?] test so that the
test is run @emph{only} when the result was not an exception.  (It's
important that the compiler is not run within the exception handler
since we don't want the compiler crashing to circumvent the test: if
the interpreter produces a value, the compiler is not allowed to
crash!)

We can confirm that the compiler is still correct on meaningful expressions:

@ex[
(check-compiler (parse #t))
(check-compiler (parse #f))
(check-compiler (parse '(if #t 1 2)))
(check-compiler (parse '(if #f 1 2)))
(check-compiler (parse '(if 0 1 2)))
(check-compiler (parse '(if 7 1 2)))
(check-compiler (parse '(if (zero? 7) 1 2)))]


For meaningless expressions, the compiler may produce bits that are illegal
or, even worse, simply do something by misinterpreting the
meaning of the bits:
@ex[
(eval:error (exec (parse '(add1 #f))))
(exec (parse '(if (zero? #t) 7 8)))
]

Yet these are not counter-examples to the compilers correctness:

@ex[
(check-compiler (parse '(add1 #f)))
(check-compiler (parse '(if (zero? #t) 7 8)))]


With this set up, we can randomly generate Dupe programs and throw
them at @racket[check-compiler]. Many randomly generated programs will
have type errors in them, but with our revised version
@racket[check-compiler], it won't matter (although it's worth thinking about
how well this is actually testing the compiler).

@ex[
(eval:alts (require "random.rkt") (void))
(random-expr)
(random-expr)
(random-expr)
(random-expr)
(random-expr)
(random-expr)
(for ([i (in-range 10)])
  (check-compiler (random-expr)))
]

