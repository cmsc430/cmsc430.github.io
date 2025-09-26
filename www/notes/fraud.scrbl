#lang scribble/manual

@(require (for-label (except-in racket ... compile) a86/ast))
@(require redex/pict
          redex/reduction-semantics
          racket/runtime-path
          scribble/examples
	  "utils.rkt"
	  "ev.rkt"
	  "../utils.rkt")


@(define-language L (e ::= ignored))
@(define-extended-language F-let L
  (e ::= .... x (let ((x e)) e))
  (x ::= variable))

@(define-extended-language F-prim2 L
  (e ::= .... (p2 e_1 e_2))
  (p2 ::= + - = <))

@(define codeblock-include (make-codeblock-include #'h))

@(ev '(require rackunit a86 fraud fraud/translate))

@(define this-lang "Fraud")
@(define prefix (string-append this-lang "-"))

@title[#:tag this-lang]{@|this-lang|: local binding, variables, and binary operations}

@src-code[this-lang]

@emph{To be is to be the value of a variable.}

@table-of-contents[]

@section[#:tag-prefix prefix]{Binding, variables, and binary operations}

Let's now consider add a notion of @bold{local binding} and
the ability to use @bold{binary operations} to our target
language.


We'll call it @bold{@this-lang}.

First, let's deal with the issue of variables and variable bindings.

We will use the following syntax to bind local variables:

@racketblock[
(let ((_id0 _e0))
  _e)
]

This form binds the identifier @racket[_id0] to value of @racket[_e0]
within the scope of @racket[_e].

This is a specialization of Racket's own local binding form, which
allows for any number of bindings to be made with @racket[let]:

@racketblock[
(let ((_id0 _e0) ...)
  _e)
]

We adopt this specialization of Racket's @racket[let] syntax so that
you can always take a @this-lang program and run it in Racket to confirm
what it should produce.

Adding a notion of variable binding also means we need to add
variables to the syntax of expressions.

Together this leads to the following grammar for @|this-lang|:

@centered{@render-language[F-let]}

Which can be modeled with the following data type definition:

@filebox-include-fake[codeblock "fraud/ast.rkt"]{
#lang racket
;; type Expr =
;; ...
;; | (Let Id Expr Expr)
;; | (Var Id)
;; type Id  = Symbol
...
(struct Let (x e1 e2) #:prefab)
(struct Var (x) #:prefab)
}


Now for binary operations...


You may have noticed that up until this point, evaluating
compound expressions in our language always depend upon the
result of a single subexpression. For example,
@racket[(add1 _e)] depends upon the result of @racket[_e],
@racket[(zero? _e)] depends upon @racket[_e], and so on.
Even expressions that involve multiple subexpressions such
as @racket[(if _e0 _e1 _e2)] really only depend on
@racket[_e0] to determine which of @racket[_e1] or
@racket[_e2] to evaluate. And in the case of
@racket[(begin _e0 _e1)], first we determine the value of
@racket[_e0] and then @racket[_e1], but these values are
never combined in any way.


Let's now consider what happens when we have @bold{multiple
subexpressions} whose results must be combined in order to evaluate an
expression.  As an example, consider @racket[(+ _e0 _e1)].  We must
evaluate @emph{both} @racket[_e0] and @racket[_e1] and sum their
results.

What's new are the following @emph{binary} operations:

@racketblock[
(+ _e0 _e1)
(- _e0 _e1)
(< _e0 _e1)
(= _e0 _e1)
]

This leads to the following additions to the grammar for @|this-lang|:

@centered[(render-language F-prim2)]

We can model it as a datatype as usual:

@filebox-include-fake[codeblock "fraud/ast.rkt"]{
#lang racket
;; type Expr =
;; ...
;; | (Prim2 Op2 Expr Expr)
;; type Op2 = '+ | '- | '< | '=
...
(struct Prim2 (p e1 e2) #:prefab)
}



@section[#:tag-prefix prefix]{Syntax matters}

With the introduction of variables comes the issue of expressions that
have @bold{free} and @bold{bound variables}.  A bound variable is a
variable that occurs within the scope of some binder for that
variable.  A free variable is one that occurs within an expression
that does not bind that variable.  So for example, in @racket[(let ((x
5)) (add1 x))], the occurrence of @racket[x] is bound because it
occurs within the body of a @racket[let] expression that binds
@racket[x].  Although, if the expression were just @racket[(add1 x)],
then the occurrence of @racket[x] is free (or unbound).  Note that an
expression might include both free and bound occurrences of a
variable, e.g. in @racket[(+ (let ((x 5)) x) x)] there are two
occurrences of @racket[x]: one is bound and one is free.

An important set of expressions are those that contain no free
variables.

@#reader scribble/comment-reader
(racketblock
;; type ClosedExpr = { e ∈ Expr | e contains no free variables }
)

This set is important because only closed expressions should be
interpreted (or compiled).  We will consider free variables to be a
syntax error.  To this end, we provide two versions of the parser.
The @racket[parse-closed] parser raises an error when it encounters an
unbound variable and therefore guarantees to always produce an element
of @tt{ClosedExpr}.  The @racket[parse] parser on the other hand
produces elements of @tt{Expr} and parse unbound variables as
variables:

@ex[
(parse 'x)
(parse '(add1 x))
(parse '(let ((x 5)) (add1 x)))
(eval:error (parse-closed 'x))
(eval:error (parse-closed '(add1 x)))
(parse-closed '(let ((x 5)) (add1 x)))]

Another issue that comes up now is the potential to bind variables
that conflict with other keywords used in the language such as
@racket[add1], @racket[sub1], @racket[if], etc.  Racket, following
Scheme, adopts a flexible approach that allows variables bindings to
shadow @emph{any} keyword in the language.  We can do the same.

This means that parsing an expression depends on the binding structure
parsed so far.  For example @racket[add1] might be a variable
occurrence if it appears in a context that binds the variable
@racket[add1].  The parser has been updated to take this in to account.
Consider the following examples:

@ex[
(parse '(let ((add1 1)) (sub1 add1)))
(eval:error (parse '(let ((add1 1)) (add1 add1))))
(parse '(let ((let 1)) let))
(parse 'let)]


The heart of this revised parsing strategy is the function
@racket[parse/acc] with takes an s-expression to be parsed into an
expression as well as a list of bound and free variables.  It computes
both the parsed expression and a list of variables that occur free in
it.

When encountering a keyword like @racket[if], @racket[let], etc., we
check that it is not in the set of bound variables before parsing the
input as that particular form of expression.  When we encounter a
variable occurrence, if it is not in the set of bound variables, we
add it to the set of free variables in the result.  When binding a
variable, we add it to the set of bound variables before parsing the
relevant part of the input where the variable is bound:

@codeblock-include["fraud/parse.rkt"]


@section[#:tag-prefix prefix]{Meaning of @this-lang programs}

The meaning of @this-lang programs depends on the form of the expression and
in the case of integers, increments, and decrements, the meaning is
the same as in the prior languages.

The two new forms are let-expressions and variables.

@itemlist[

@item{the meaning of a let expression @racket[(let ((_x _e0))
_e)] is the meaning of @racket[_e] (the @bold{body} of the @racket[let])
when @racket[_x] means the value of @racket[_e0] (the @bold{right hand
side} of the @racket[let]),}

@item{the meaning of a variable @racket[_x] depends on the context in
which it is bound.  It means the value of the right-hand side of the
nearest enclosing @racket[let] expression that binds @racket[_x].  If
there is no such enclosing @racket[let] expression, the variable is
meaningless.}

]

Let's consider some examples:

@itemlist[

@item{@racket[x]: this expression is meaningless on its own.}

@item{@racket[(let ((x 7)) x)]: this means @racket[7], since the body
expression, @racket[x], means @racket[7] because the nearest enclosing binding for
@racket[x] is to @racket[7], which means @racket[7].}

@item{@racket[(let ((x 7)) 2)]: this means @racket[2] since the body
expression, @racket[2], means @racket[2].}

@item{@racket[(let ((x 7)) (add1 x))]: this means @racket[8] since the
body expression, @racket[(add1 x)], means one more than @racket[x] and @racket[x]
means @racket[7] because the nearest enclosing binding for @racket[x] is to
@racket[7].}

@item{@racket[(let ((x (add1 7))) x)]: this means @racket[8] since the
body expression, @racket[x], means @racket[8] because the nearest
enclosing binding for @racket[x] is to @racket[(add1 7)] which means
@racket[8].}

@item{@racket[(let ((x 7)) (let ((y 2)) x))]: this means @racket[7] since the body
expression, @racket[(let ((y 2)) x)], means @racket[2] since the body expression,
@racket[x], means @racket[7] since the nearest enclosing binding for @racket[x] is to
@racket[7].}

@item{@racket[(let ((x 7)) (let ((x 2)) x))]: this means 2 since the
body expression, @racket[(let ((x 2)) x)], means @racket[2] since the
body expression, @racket[x], means @racket[2] since the nearest
enclosing binding for @racket[x] is to @racket[2].}

@item{@racket[(let ((x (add1 x))) x)]: this is meaningless, since the
right-hand side expression, @racket[(add1 x)] is meaningless because
@racket[x] has no enclosing @racket[let] that binds it.}

@item{@racket[(let ((x 7)) (let ((x (add1 x))) x))]: this means
@racket[8] because the body expression @racket[(let ((x (add1 x))) x)]
means @racket[8] because the body expression, @racket[x], is bound to
@racket[(add1 x)] is in the nearest enclosing @racket[let] expression
that binds @racket[x] and @racket[(add1 x)] means @racket[8] because
it is one more than @racket[x] where @racket[x] is bound to @racket[7]
in the nearest enclosing @racket[let] that binds it.}

]

Make sure you have a good understanding of how binding work in these
examples before moving on.  Remember: you can always check your
understanding by pasting expressions into Racket and seeing what it
produces, or better yet, write examples in DrRacket and hover over
identifiers to see arrows between variable bindings and their
occurrences.

One thing that should be clear from these examples is that the meaning
of a sub-expression is not determined by the form of that expression
alone.  For example, @tt{x} could mean 7, or it could mean 8, or it
could be meaningless, or it could mean 22, etc.  It depends on the
context in which it occurs.  So in formulating the meaning of an
expression, this context must be taken into account.

Thinking more about what information we need to keep track
of reveals that when considering the meaning of a
@racket[let]'s body, we need to know that the variable it's
binding means the value of the right-hand expression. Since
a program potentially consists of nested @racket[let]
expressions, we will need to keep track of some number of
pairs of variables and their meaning. We will refer to this
contextual information as an @bold{environment}.

The meaning of a variable is resolved by looking up its
meaning in the environment. The meaning of a @racket[let]
will depend on the meaning of its body with an extended
environment that associates its variable binding to the
value of the right hand side.



The heart of the semantics is a function @racket[interp-e] the
provides the meaning of an expression under a given environment.  The
top-level @racket[interp] function simply calls @racket[interp-e]
with an empty enivornment.

These rely on two functions: one for extending an environment with a
variable binding and one for lookup up a variable binding in an
environment.


With the semantics of @racket[let] and variables out of the
way, extending the @this-lang semantics to hand binary
operations is pretty straightforward. For
@racket[(+ _e0 _e1)], the meaning is the sum of the meanings
of @racket[_e0] and @racket[_e1], when they mean integers,
otherwise the meaning is an error.



It is defined by structural recursion on the expression.  Environments
are represented as lists of associations between variables and values.
There are two helper functions for @racket[ext] and @racket[lookup]:

@codeblock-include["fraud/interp.rkt"]

We can confirm the interpreter computes the right result for the
examples given earlier:

@ex[
(interp (parse '(let ((x 7)) x)))
(interp (parse '(let ((x 7)) 2)))
(interp (parse '(let ((x 7)) (add1 x))))
(interp (parse '(let ((x (add1 7))) x)))
(interp (parse '(let ((x 7)) (let ((y 2)) x))))
(interp (parse '(let ((x 7)) (let ((x 2)) x))))
(interp (parse '(let ((x 7)) (let ((x (add1 x))) x))))
(interp (parse '(+ 3 4)))
(interp (parse '(+ 3 (+ 2 2))))
(interp (parse '(+ #f 8)))
]


@section[#:tag-prefix prefix]{Lexical Addressing}

Just as we did with @seclink["Dupe"], the best way of understanding
the forthcoming compiler is to write a ``low-level'' interpreter that
explains some of the ideas used in the compiler without getting bogged
down in code generation details.

At first glance at @racket[interp], it would seem we will need to
generate code for implementing the @tt{REnv} data structure and its
associated operations: @racket[lookup] and @racket[ext].  @tt{REnv} is
an inductively defined data type, represented in the interpreter as a
list of lists.  Interpreting a variable involves recursively scanning
the environment until the appropriate binding is found.  This would
take some doing to accomplish in x86.

However...

It is worth noting some invariants about environments created during
the running of @racket[interp].  Consider some subexpression
@racket[_e] of the program.  What environment will be used whenever
@racket[_e] is interpreted?  Well, it will be a mapping of every
variable bound outside of @racket[_e].  It's not so easy to figure out
@emph{what} these variables will be bound to, but the skeleton of the
environment can be read off from the program structure.

For example:

@racketblock[
(let ((x ...))
  (let ((y ...))
    (let ((z ...))
      _e)))
]

The subexpression @racket[_e] will @emph{always} be evaluated in an
environment that looks like:
@racketblock[
'((z ...) (y ...) (x ...))
]

Moreover, every free occurrence of @racket[x] in @racket[_e] will
resolve to the value in the third element of the environment; every
free occurrence of @racket[y] in @racket[_e] will resolve to the
second element; etc.

This suggests that variable locations can be resolved
@emph{statically} using @bold{lexical addresses}.  The lexical address
of a variable is the number of @racket[let]-bindings between the
variable occurrence and the @racket[let] that binds it.

So for example:

@itemlist[

@item{@racket[(let ((x ...)) x)]: the occurrence of @racket[x] has a
lexical address of @racket[0]; there are no bindings between the
@racket[let] that binds @racket[x] and its occurrence.}

@item{@racket[(let ((x ...)) (let ((y ...)) x))]: the occurrence of
@racket[x] has a lexical address of @racket[1] since the
@racket[let]-binding of @racket[y] sits between the
@racket[let]-binding of @racket[x] and its occurrence.}

]

We can view a variable @emph{name} as just a placeholder for the
lexical address; it tells us which binder the variable comes from.

Using this idea, let's build an alternative interpreter that operates
over an intermediate form of expression that has no notion of
variables, but just lexical addresses:

@#reader scribble/comment-reader
(racketblock
;; type IExpr =
;; | (Lit Datum)
;; | (Eof)
;; | (Prim0 Op0)
;; | (Prim1 Op1 IExpr)
;; | (Prim2 Op2 IExpr IExpr)
;; | (If IExpr IExpr IExpr)
;; | (Begin IExpr IExpr)
;; | (Let '_ IExpr IExpr)
;; | (Var Addr)
;; type Addr = Natural
)

Notice that variables have gone away, replaced by a @racket[(Var
Natural)] form.  The @racket[let] binding form no longer binds a
variable name either.

The idea is that we will translate expression (@tt{Expr}) like:

@racketblock[
(Let 'x (Lit 7) (Var 'x))]

into intermediate expressions (@tt{IExpr}) like:

@racketblock[
(Let '_ (Lit 7) (Var 0))
]

And:

@racketblock[
(Let 'x (Lit 7) (Let 'y (Lit 9) (Var 'x)))
]

into:

@racketblock[
(Let '_ (Lit 7) (Let '_ (Lit 9) (Var 1)))
]


Similar to how @racket[interp] is defined in terms of a helper
function that takes an environment mapping variables to their value,
the @racket[translate] function will be defined in terms of a helper
function that takes an environment mapping variables to their lexical
address.

The lexical environment will just be a list of variable names.  The
address of a variable occurrence is count of variable names that occur
before it in the list.  When a variable is bound (via-@racket[let])
the list grows:

@codeblock-include["fraud/translate.rkt"]

Notice that @racket[translate] is a kind of mini-compiler that
compiles @tt{Expr}s to @tt{IExpr}s.  It's only job is to eliminate
variable names by replacing variable occurrences with their lexical
addresses.  It does a minor amount of syntax checking while it's at it
by raising a (compile-time) error in the case of unbound variables.

We can try out some examples to confirm it works as expected.

@ex[
 (translate (Let 'x (Lit 7) (Var 'x)))
 (translate (Let 'x (Lit 7) (Let 'y (Lit 9) (Var 'x))))
 ]

The interpreter for @tt{IExpr}s will still have an
environment data structure, however it will be simpler than
the association list we started with. The run-time
environment will consist only of a list of values; the
lexical address of (what used to be a) variable indicates
the position in this list. When a value is bound by a
@racket[let], the list grows:

@codeblock-include["fraud/interp-lexical.rkt"]

Try to convince yourself that the two version of @racket[interp]
compute the same function.


@section[#:tag-prefix prefix]{Compiling lets and variables}

Suppose we want to compile @racket[(let ((x 7)) (add1 x))].  There
are two new forms we need to compile: the @racket[(let ((x ...))
...)] part and the @racket[x] part in the body.

We already know how to compile the @racket[(add1 ...)] part and the
@racket[7] part.

What needs to happen?  Compiling the @racket[7] part will emit
instructions that, when run, leave @racket[7] in the @racket['rax]
register.  Compiling the @racket[(add1 ...)] part relies on the
result of evaluating it's subexpression to be in @racket['rax] when it
increments it.  So, compile the variable binding needs to stash the
@racket[7] somewhere and compiling the variable occurrence needs to
retrieve that stashed value.  After the let expression has been run,
the stashed value should go away since the variable is no longer in
scope.

This ``stashing'' of values follows a stack discipline.  When entering
a let, after the right-hand side has been run, the result should be
pushed.  When evaluating a variable occurrence, the bound value is on
the stack.  After exiting the let, the stack can be popped.

Suppose we want to compile @racket[(let ((x 7)) (let ((y 2)) (add1
x)))].  Using the intuition developed so far, we should push 7, push
2, and then run the body.

@#reader scribble/comment-reader
(racketblock
(seq (Mov 'rax (values->bits 7))
     (Push 'rax)
     (Mov 'rax (values->bits 2))
     (Push 'rax)))

But notice that the value of @racket[x] is no longer on the top of the
stack; @racket[y] is.  So to retrieve the value of @racket[x] we need
skip past the @racket[y].  But calculating these offsets is pretty
straightforward.  In this example there is one binding between the
binding of @racket[x] and this occurrence, so to reference @racket[x],
load the second element on the stack.  The complete expression can be
compiled as:

@#reader scribble/comment-reader
(racketblock
(seq (Mov 'rax (values->bits 7))
     (Push 'rax)
     (Mov 'rax (values->bits 2))
     (Push 'rax)
     ; (add1 x)
     (Mov 'rax (Offset 'rsp 8))
     (Add 'rax (value->bits 1))))

When the @racket[let] expression is complete, the bindings for
@racket[x] and @racket[y] need to be popped off the stack.  To do so,
we can simply increment @racket['rsp] since the values of @racket[x]
and @racket[y] are irrelevant.

@#reader scribble/comment-reader
(racketblock
;; complete compilation of (let ((x 7)) (let ((y 2)) (add1 x))):
(seq (Mov 'rax (values->bits 7))
     (Push 'rax) ; bind x to 7
     (Mov 'rax (values->bits 2))
     (Push 'rax) ; bind y to 2
     (Mov 'rax (Offset 'rsp 8)) ; ref x
     (Add 'rax (value->bits 1)) ; add1
     (Add 'rsp 8)  ; pop y
     (Add 'rsp 8)) ; pop x
)

Since we push every time we enter a let and pop every time we leave,
the number of bindings between an occurrence and its binder is exactly
the offset from the top of the stack we need use; in other words, the
compiler uses lexical addresses just like the alternative interperter
above and the stack of values plays the role of the run-time
envivornment.

This means the compiler will need to use a compile-time environment to
track bound variables and make use of a @racket[lookup] function to
compute the lexical address of variable references just like the
interpreter.  The only (trivial) difference is the addresses are given
in word offsets, i.e. each binding adds @racket[8] to the address.

@section[#:tag-prefix prefix]{Compiling binary operations}

Binary expressions are easy to deal with at the level of the semantics
and interpreter.  However things are more complicated at the level of
the compiler.

To see the problem consider blindly following the pattern we used (and
ignoring type errors for the moment):

@#reader scribble/comment-reader
(racketblock
;; Expr Expr CEnv -> Asm
(define (compile-+ e0 e1 c)
  (seq (compile-e e0 c)
       (compile-e e1 c)
       (Add 'rax _????)))
)

The problem here is that executing @racket[_e0] places its result in
register @racket['rax], but then executing @racket[_e1] places its
result in @racket['rax], overwriting the value of @racket[_e0].

It may be tempting to use another register to stash away the result of
the first subexpression:

@#reader scribble/comment-reader
(racketblock
;; Expr Expr CEnv -> Asm
(define (compile-+ e0 e1 c)
  (seq (compile-e e0 c)
       (Mov 'r8 'rax)
       (compile-e e1 c)
       (Add 'rax 'r8)))
)

Can you think of how this could go wrong?

To come up with a general solution to this problem, we need to save
the result of @racket[_e0] and then retrieve it after computing
@racket[_e1] and it's time to sum.

Note that this issue only comes up when @racket[_e0] is a
@bold{serious} expression, i.e. an expression that must do some
computation.  If @racket[_e0] were a literal integer or a variable, we
could emit working code.  For example:


@#reader scribble/comment-reader
(racketblock
;; Integer Expr CEnv -> Asm
;; A special case for compiling (+ i0 e1)
(define (compile-+-int i0 e1 c)
  (seq (compile-e e1 c)
       (Add 'rax (value->bits i0))))

;; Id Expr CEnv -> Asm
;; A special case for compiling (+ x0 e1)
(define (compile-+-var x0 e1)
  (let ((i (lookup x0 c)))
    (seq (compile-e e1 c)
         (Add 'rax (Offset 'rsp i)))))
)

The latter suggests a general solution could be to transform binary
primitive applications into a @racket[let] form that binds the first
subexpression to a variable and then uses the @racket[compile-+-var]
function above.  The idea is that every time the compiler encounters
@racket[(+ _e0 _e1)], we transform it to @racket[(let ((_x _e0)) (+ _x
_e1))].  For this to work out, @racket[_x] needs to be some variable
that doesn't appear free in @racket[_e1].  This transformation is
what's called @bold{ANF} (administrative normal form) and is a widely
used intermediate representation for compilers.


But, we can also solve the problem more directly by considering the
code that is generated for the ANF style expression above.

Consider the lexical address of @racket[_x] in the transformed code
above.  It is @emph{always} 0 because the transformation puts the
@racket[let] immediately around the occurrence of @racket[_x].  So if
we're compiling @racket[(+ _e0 _e1)] in environment @racket[_c] using
this approach, we know the value of @racket[_e0] will live at
@racket[(Offset 'rsp 0)].  There's no need for a
@racket[let] binding or a fresh variable name.  And this observation
enables us to write a general purpose compiler for binary primitives
that doesn't require any program transformation: we simply push the
value of @racket[_e0] on the top of the stack and retrieve it later.

Here is a first cut:

@#reader scribble/comment-reader
(racketblock
;; Expr Expr CEnv -> Asm
(define (compile-+ e0 e1 c)
  (let ((x (gensym))) ; generate a fresh variable
    (seq (compile-e e0 c)
         (Push 'rax)
         (compile-e e1 (cons x c))
         (Pop 'r8)
         (Add 'rax 'r8)))) 
)

There are a couple things to notice.  When
compiling @racket[_e1] in environment @racket[(cons x c)], we know
that no variable in @racket[_e1] resolves to @racket[x] because
@racket[x] is a freshly @racket[gensym]'d symbol.  Putting (an
unreferenced) @racket[x] in the environment serves only to ``bump up''
by one the offset of any variable bound after @racket[x] so as to not
override the spot where @racket[e0]'s values lives.  We can accomplish
the same thing by sticking in something that no variable is equal to:
@racket[#f]:

@#reader scribble/comment-reader
(racketblock
(define (compile-+ e0 e1 c)
  (seq (compile-e e0 c)
       (Push 'rax)
       (compile-e e1 (cons #f c))
       (Pop 'r8)
       (Add 'rax 'r8)))
)

With variables, @racket[let]s, and binary operations in place, we can
complete the compiler.

@section[#:tag-prefix prefix]{The wrinkle of stack alignment}

There is a wrinkle that comes from using the stack to hold variable
bindings and intermediate results, which has to do with how it
interacts with our previous language features.  In particular, we were
previously able to take a fairly simple approach to implement calls to
C functions in the runtime system.

Recall from @secref{calling-c} that the stack needs to be 16-byte
aligned before issuing a @racket[Call] instruction.  Since the runtime
calls the @racket['entry] label to execute the code emitted by the
compiler, and this pushes an 8-byte address on the stack, the stack is
misaligned at the entry to our code.  Our solution was to immediately
subtract 8 from @racket['rsp] at entry and add back 8 just before
returning.  This way any calls, such as calls to @tt{read_byte},
@tt{write_byte}, or even @tt{raise_error} were in an aligned state.

This simple approach worked since the code emitted by the compiler
never modified the stack (other than making calls).  But now what
happens since code makes frequent use of the stack?

Consider the following two, seemingly equivalent, examples:

@itemlist[
@item{@racket[(write-byte 97)]}
@item{@racket[(let ((x 97)) (write-byte x))]}]

Assuming the stack is aligned before making the call to the C function
@tt{write_byte} in the first example, means that it will be misaligned
in second example, since the code would have first @racket[Push]ed
@racket[97] on the stack.  Symmetrically, if the stack were aligned in
the second example, then it couldn't be in the first.

So our previous once-and-done solution to the stack alignment issue
will no longer work.  Instead, we will have to emit code that aligns
the stack at every @racket[Call] and this adjustment will depend upon
the state of the stack pointer when the call occurs.

It's possible to compute the needed adjustment to the stack statically
using the compile-time environment, however we opt for a simpler,
one-size-fits-all approach of @emph{dynamically} aligning the stack
immediately before issuing a @racket[Call].  We do this by emitting
code that adjusts the stack pointer based on its current value,
computing a pad: either @racket[0] or @racket[8], which is subtracted
from @racket['rsp].  We then stash this pad value away in a
non-volatile register, which means the called function is not allowed
to modify it---or at least, if they do, they must restore before
returning.  When the call returns, we add the pad value back to
@racket['rsp] to restore the stack pointer to where it was before
being aligned.

Note that stack pointer is either divisible by 16 (meaning the last
four bits are @racket[0]) or by 8 but not by 16 (meaning the last four
bits are @binary[8]).  When the stack pointer is divisible by 16, it's
aligned and no adjustment is needed.  Otherwise we need to adjust by
subtracting 8.

Here is the code we can use to pad and unpad the stack.  It does an
@racket[And] of the stack address and @binary[8], saving the result
into @racket['r15].  So @racket['r15] is @racket[0] when @racket['rsp]
is aligned and @racket[8] when misaligned.  In both cases, subtracting
@racket['r15] from @racket['rsp] ensures @racket['rsp] is aligned.
The @racket[unpad-stack] simply adds @racket['r15] back.

@#reader scribble/comment-reader
(racketblock
;; Asm
;; Dynamically pad the stack to be aligned for a call
(define pad-stack
  (seq (Mov r15 rsp)
       (And r15 #b1000)
       (Sub rsp r15)))

;; Asm
;; Undo the stack alignment after a call
(define unpad-stack
  (seq (Add rsp r15)))
)

Since @racket['r15] is a @emph{non-volatile} register, meaning a
called function must preserve its value when returning, it's safe to
stash away the stack adjustment value in this register.

We can now call C functions by first padding then unpadding the stack:

@#reader scribble/comment-reader
(racketblock
(seq pad-stack
     (Call 'read_byte)
     unpad-stack))

Signalling errors is likewise complicated and we handle it by having a
target @racket['raise_error_align] that aligns the stack using
@racket[pad-stack] before calling @racket['raise_error].  It doesn't
bother with @racket[unpad-stack] because there's no coming back.

Here is the compiler for primitives that incorporates all of these
stack-alignment issues, but is otherwise the same as before:

@filebox-include[codeblock fraud "compile-ops.rkt"]

@section[#:tag-prefix prefix]{Complete @this-lang compiler}

We can now take a look at the main compiler for expressions.  Notice
the compile-time environment which is weaved through out the
@racket[compile-e] function and its subsidiaries, which is critical in
@racket[compile-variable] and extended in @racket[compile-let]. 

@filebox-include[codeblock fraud "compile.rkt"]

Notice that the @racket[lookup] function computes a lexical
address from an identifier and compile-time environment,
just like the @racket[lexical-address] function in @tt{
 translate.rkt}. The only difference is addresses are
calculated as byte offsets, hence the addition of 8 instead
of 1 in the recursive case.

Let's take a look at some examples of @racket[let]s and variables:

@ex[
(define (show e c)
  (compile-e (parse e) c))
(show 'x '(x))
(show 'x '(z y x))
(show '(let ((x 7)) x) '())
(show '(let ((x 7)) 2) '())
(show '(let ((x 7)) (add1 x)) '())
(show '(let ((x (add1 7))) x) '())
(show '(let ((x 7)) (let ((y 2)) x)) '())
(show '(let ((x 7)) (let ((x 2)) x)) '())
(show '(let ((x 7)) (let ((x (add1 x))) x)) '())
]

And running the examples:
@ex[
(exec (parse '(let ((x 7)) x)))
(exec (parse '(let ((x 7)) 2)))
(exec (parse '(let ((x 7)) (add1 x))))
(exec (parse '(let ((x (add1 7))) x)))
(exec (parse '(let ((x 7)) (let ((y 2)) x))))
(exec (parse '(let ((x 7)) (let ((x 2)) x))))
(exec (parse '(let ((x 7)) (let ((x (add1 x))) x))))
]

Here are some examples of binary operations:

@ex[
(show '(+ 1 2) '())
(show '(+ (+ 3 4) (+ 1 2)) '())
(show '(+ x y) '(x y))
]

And running the examples:
@ex[
(exec (parse '(+ 1 2)))
(exec (parse '(+ (+ 3 4) (+ 1 2))))
(exec (parse '(let ((y 3)) (let ((x 2)) (+ x y)))))
]

Finally, we can see the stack alignment issues in action:

@ex[
(show '(write-byte 97) '())
(show '(write-byte 97) '(x))
(show '(add1 #f) '())
(show '(add1 #f) '(x))
]

@section[#:tag-prefix prefix]{Correctness}

For the statement of compiler correctness, we must now restrict the
domain of expressions to be just @bold{closed expressions}, i.e. those
that have no unbound variables.

@bold{Compiler Correctness}: @emph{For all @racket[e] @math{∈}
@tt{ClosedExpr}, @racket[i], @racket[o] @math{∈} @tt{String}, and @racket[v]
@math{∈} @tt{Value}, if @racket[(interp/io e i)] equals @racket[(cons
v o)], then @racket[(exec/io e i)] equals
@racket[(cons v o)].}

The check for correctness is the same as before, although the check should only be applied
to elements of @tt{ClosedExpr}:

@filebox-include[codeblock fraud "correct.rkt"]

