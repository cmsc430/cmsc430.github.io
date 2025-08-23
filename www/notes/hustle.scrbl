#lang scribble/manual

@(require (for-label (except-in racket ... compile) a86))
@(require redex/pict
          racket/runtime-path
          scribble/examples
	  (except-in hustle/semantics ext lookup)
          (prefix-in sem: (only-in hustle/semantics ext lookup))
	  "../fancyverb.rkt"
	  "utils.rkt"
	  "ev.rkt"
	  "../utils.rkt")

@(define codeblock-include (make-codeblock-include #'h))

@(ev '(require rackunit a86))
@(ev `(current-directory ,(path->string (build-path langs "hustle"))))
@(void (ev '(with-output-to-string (thunk (system "make runtime.o")))))
@(for-each (λ (f) (ev `(require (file ,f))))
	   '("main.rkt" "heap.rkt" "unload.rkt" "interp-prims-heap.rkt"))

@(define this-lang "Hustle")

@title[#:tag this-lang]{@|this-lang|: heaps and lists}

@src-code[this-lang]

@emph{A little and a little, collected together, become a great deal;
the heap in the barn consists of single grains, and drop and drop
makes an inundation.}

@table-of-contents[]

@section{Inductive data}

So far all of the data we have considered can fit in a single machine
word (64-bits).  Well, integers can't, but we truncated them and only
consider, by fiat, those integers that fit into a register.

In the @bold{@this-lang} language, we will add two @bold{inductively
defined data types}, boxes and pairs, which will require us to relax
this restriction.

Boxes are like unary pairs, they simply hold a value, which can be
projected out.  Pairs hold two values which each can be projected out.

To see how values are now inductively defined notice that if you have
a value @racket[_v], you can make another value with @racket[(box _v)].
Similarly, if @racket[_v1] and @racket[_v2] are values, then so is
@racket[(cons _v1 _v2)].  This suggests the following recursive type
definition for values:

@#reader scribble/comment-reader
(racketblock
;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void
;; | '()
;; | (cons Value Value)
;; | (box Value)
)


The new operations include constructors @racket[(box _e)] and
@racket[(cons _e0 _e1)] and projections @racket[(unbox _e)],
@racket[(car _e)], and @racket[(cdr _e)].  We'll also include
predicates for identifying boxes and pairs: @racket[(box? _e)] and
@racket[(cons? _e)].

These features will operate like their Racket counterparts:
@ex[
(unbox (box 7))
(car (cons 3 4))
(cdr (cons 3 4))
(box? (box 7))
(cons? (cons 3 4))
(box? (cons 3 4))
(cons? (box 7))
]

@margin-note{Usually boxes are @emph{mutable} data structures, like
OCaml's @tt{ref} type, but we will examine this aspect later.  For now,
we treat boxes as immutable data structures.}

We will also add support for writing pair and box @emph{literals}
using the same @racket[quote] notation that Racket uses.

These features will operate like their Racket counterparts:
@ex[
(unbox '#&7)
(car '(3 . 4))
(cdr '(3 . 4))
(box? '#&7)
(cons? '(3 . 4))
(box? '(3 . 4))
(cons? '#&7)
]

@section{Empty lists can be all and end all}

While we've introduced pairs, you may wonder what about @emph{lists}?
Just as in Racket, lists can be represented by idiomatic uses of
@racket[cons]: a non-empty list is a pair whose @racket[car] is an
element and whose @racket[cdr] is the rest of the list.  What's left?
We need a representation of the empty list!

In Racket, and in our languages, we write this value as @racket['()].
There's nothing particularly special about the empty list value, we
just need another distinguished value to designate it.

Using @racket[cons] and @racket['()] in a structured way we can form
@emph{proper list}, among other useful data structures.

We use the following AST data type for @|this-lang|:

@filebox-include-fake[codeblock "hustle/ast.rkt"]{
#lang racket
;; type Expr = ... | (Lit Datum)
;; type Datum = ... | (cons Datum Datum) | (box Datum) | '()
;; type Op1 = ... | 'box | 'car | 'cdr | 'unbox | 'box? | 'cons?
;; type Op2 = ... | 'cons
}

@section{Parsing}

Mostly the parser updates for @|this-lang| are uninteresting.  The
only slight twist is the addition of compound literal datums.

It's worth observing a few things about how @racket[quote] works in
Racket.  First, some datums are @emph{self-quoting}, i.e. we can
write them with or without quoting and they mean the same thing:
@ex[
5
'5]

All of the datums consider prior to @|this-lang| have been self-quoting:
booleans, integers, and characters.

Of the new datums, boxes are self-quoting, but pairs and the empty
list are not.
@ex[
#&7
'#&7
(eval:error ())
'()
(eval:error (1 . 2))
'(1 . 2)]

The reason for this is that unquoted list datums would be confused
with expression forms without the @racket[quote], so its required,
however for the other datums, there's no possible confusion and the
@racket[quote] is inferred.  Note also that once inside a self-quoting
datum, it's unambiguous that we're talking about literal data and not
expressions that need to be evaluated, so you can have empty lists and
pairs:
@ex[
#&()
#&(1 . 2)]

This gives rise to two notions of datums that our parser uses,
with (mutually defined) predicates for each:

@filebox-include-fake[codeblock "hustle/parse.rkt"]{
;; Any -> Boolean
(define (self-quoting-datum? x)
  (or (exact-integer? x)
      (boolean? x)
      (char? x)
      (and (box? x) (datum? (unbox x)))))

;; Any -> Boolean
(define (datum? x)
  (or (self-quoting-datum? x)
      (empty? x)
      (and (cons? x) (datum? (car x)) (datum? (cdr x)))))
}

Now when the parser encounters something that is a self-quoting datum,
it can parse it as a @racket[Lit].  But for datums that are quoted, it
will need to recognize the @racket[quote] form, so anything that has
the s-expression shape @racket[(quote d)] will also get parsed as a
@racket[Lit].

Things can get a little confusing here so let's look at some examples:
@ex[
(parse 5)
(parse '5)
]

Here, both examples are really the same.  When we write @racket['5],
that @racket[read]s it as @racket[5], so this is really the same
example and corresponds to an input program that just contains the
number @racket[5] and we are calling @racket[parse] with an argument
of @racket[5].

If the input program contained a quoted @racket[5], then it would be
@racket['5], which we would represent as an s-expression as
@racket[''5].  Note that this reads as @racket['(quote 5)], i.e. a
two-element list with the symbol @racket['quote] as the first element
and the number @racket[5] as the second. So when writing examples
where the input program itself uses @racket[quote] we will see this
kind of double quotation, and we are calling @racket[parse] with
a two-element list as the argument:

@ex[
(parse ''5)]

This is saying that the input program was @racket['5].  Notice that it
gets parsed the same as @racket[5] by our parser.

If we were to parse the empty list, this should be considered a parse
error because it's like writing @racket[()] in Racket; it's not a valid
expression form:

@ex[
(eval:error (parse '()))]

However, if the empty list is quoted, i.e. @racket[''()], then we are
talking about the expression @racket['()], so this gets parsed as
@racket[(Lit '())]:

@ex[
(parse ''())]

It works similarly for pairs:

@ex[
(eval:error (parse '(1 . 2)))
(parse ''(1 . 2))]

While these examples can be a bit confusing at first, implementing
this behavior is pretty simple.  If the input is a
@racket[self-quoting-datum?], then we parse it as a @racket[Lit]
containing that datum.  If the the input is a two-element list of the
form @racket[(list 'quote _d)] and @racket[_d] is a @racket[datum?],
the we parse it as a @racket[Lit] containing @racket[_d].

Note that @emph{if} the examples are confusing, the parser actually
explains what's going on in Racket. Somewhere down in the code that
implements @racket[read] is something equivalent to what we've done
here in @racket[parse] for handling self-quoting and explicitly quoted
datums.  Also note that after the parsing phase, self-quoting and
quoted datums are unified as @racket[Lit]s and we no longer need to be
concerned with any distinctions that existed in the concrete syntax.

The only other changes to the parser are that we've added some new
unary and binary primitive names that the parser now recognizes for
things like @racket[cons], @racket[car], @racket[cons?], etc.

@codeblock-include["hustle/parse.rkt"]




@section{Meaning of @this-lang programs, implicitly}

To extend our interpreter, we can follow the same pattern we've been
following so far.  We have new kinds of values such as pairs, boxes,
and the empty list, so we have to think about how to represent them,
but the natural thing to do is to represent them with the
corresponding kind of value from Racket.  Just as we represent Hustle
booleans with Racket booleans, Hustle integers with Racket integers,
and so on, we can also represent Hustle pairs with Racket pairs.  We
can represent Hustle boxes with Racket boxes.  We can represent
Hustle's empty list with Racket's empty list.

Under this choice of representation, there's very little to do in
the interpreter.  We only need to update the interpretation of
primitives to account for our new primitives such as @racket[cons],
@racket[car], etc.  And how should these primitives be interpreted?
Using their Racket counterparts of course!

@codeblock-include["hustle/interp-prim.rkt"]

We can try it out:

@ex[
(interp (parse '(cons 1 2)))
(interp (parse '(car (cons 1 2))))
(interp (parse '(cdr (cons 1 2))))
(interp (parse '(car '(1 . 2))))
(interp (parse '(cdr '(1 . 2))))
(interp (parse '(let ((x (cons 1 2)))
                  (+ (car x) (cdr x)))))
]


Now while this is a perfectly good specification, this interpreter
doesn't really shed light on how constructing inductive data works
because it simply uses the mechanism of the defining language to
construct it.  Inductively defined data is easy to model in this
interpreter because we can rely on the mechanisms provided for
constructing inductively defined data at the meta-level of Racket.

The real trickiness comes when we want to model such data in an
impoverished setting that doesn't have such things, which of course is
the case in assembly.

The main challenge is that a value such as @racket[(box _v)] has a
value inside it.  Pairs are even worse: @racket[(cons _v0 _v1)] has
@emph{two} values inside it.  If each value is represented with 64
bits, it would seem a pair takes @emph{at a minimum} 128-bits to
represent (plus we need some bits to indicate this value is a pair).
What's worse, those @racket[_v0] and @racket[_v1] may themselves be
pairs or boxes.  The great power of inductive data is that an
arbitrarily large piece of data can be constructed.  But it would seem
impossible to represent each piece of data with a fixed set of bits.

The solution is to @bold{allocate} such data in memory, which can in
principle be arbitrarily large, and use a @bold{pointer} to refer to
the place in memory that contains the data.

Before tackling the compiler, let's look at an alternative version of
the interpreter that makes explicit a representation of memory and is
able to interpret programs that construct and manipulate inductive
data without itself relying on those mechanisms.

@section{Meaning of @this-lang programs, explicitly}

Let's develop an alternative interpreter that describes constructing
inductive data without itself constructing inductive data.

The key here is to describe explicitly the mechanisms of
memory allocation and dereference. Abstractly, memory can be
thought of as association between memory addresses and
values stored in those addresses. As programs run, there is
a current state of the memory, which can be used to look up
values (i.e. dereference memory) or to extend by making a
new association between an available address and a value
(i.e. allocating memory).

The representation of values changes to represent inductive data
through pointers to memory:

@#reader scribble/comment-reader
(racketblock
;; type Value* =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void
;; | '()
;; | (box-ptr  Address)
;; | (cons-ptr Address)
(struct box-ptr (i))
(struct cons-ptr (i))

;; type Address = Natural
)

Here we have two kinds of pointer values, @emph{box pointers} and
@emph{cons pointers}.  A box value is represented by an address (some
natural number) and a tag, the @racket[box-ptr] constructor, which
indicates that the address should be interpreted as the contents of a
box.  A cons is represented by an address tagged with
@racket[cons-ptr], indicating that the memory contains a pair of
values.

To model memory, we use a list of @tt{Value*} values.  When memory is
allocated, new elements are placed at the front of the list.  To model
memory locations, use the distance from the element to the end of the
list, this way addresses don't change as memory is allocated.

For example, suppose we have allocated memory to hold four values
@racket['(97 98 99 100)].  The address of 100 is 0; the address of 99
is 1; etc.  When a new value is allocated, say, @racket['(96 97 98
99)], the address of 99 is still 0, and so on.  The newly allocated
value 96 is at address 4.  In this way, memory grows toward higher
addresses and the next address to allocate is given by the size of the
heap currently in use.

@#reader scribble/comment-reader
(racketblock
;; type Heap = (Listof Value*)
)

When a program is intepreted, it results in a @tt{Value*} paired
together with a @tt{Heap} that gives meaning to the addresses in the
value, or an error:

@#reader scribble/comment-reader
(racketblock
;; type Answer* = (cons Heap Value*) | 'err
)

So for example, to represent a box @racket[(box 99)] we could have
a box value, i.e. a tagged pointer that points to memory containing 99:


@#reader scribble/comment-reader
(racketblock
(cons (list 99) (box-ptr 0)))

The value at list index 0 is 99 and the box value points to that
element of the heap and indicates it is the contents of a box.

It's possible that other memory was used in computing this result, so
we might end up with an answer like:

@#reader scribble/comment-reader
(racketblock
(cons (list 97 98 99) (box-ptr 0)))

Or:

@#reader scribble/comment-reader
(racketblock
(cons (list 97 98 99 100 101) (box-ptr 2)))

Both of which really mean the same value: @racket[(box 99)].

A pair contains two values, so a @racket[cons-ptr] should point to the
start of elements that comprise the pair.  For example, this answer
represents a pair @racket[(cons 100 99)]:

@#reader scribble/comment-reader
(racketblock
(cons (list 99 100) (cons-ptr 0)))

Note that the @racket[car] of this pair is at address 0 and the
@racket[cdr] is at address 1.

Note that we could have other things residing in memory, but so long
as the address points to same values as before, these answers mean the
same thing:

@#reader scribble/comment-reader
(racketblock
(cons (list 97 98 99 100 101) (cons-ptr 1)))

In fact, we can reconstruct a @tt{Value} from a @tt{Value*} and
@tt{Heap}:

@codeblock-include["hustle/unload.rkt"]

Which relies on our interface for heaps:

@codeblock-include["hustle/heap.rkt"]

Try it out:

@ex[
(unload-value (box-ptr 0) (list 99))
(unload-value (cons-ptr 0) (list 99 100))
(unload-value (cons-ptr 1) (list 97 98 99 100 101))]

What about nested pairs like @racket[(cons 1 (cons 2 (cons 3 '())))]?
Well, we already have all the pieces we need to represent values like
these.

@ex[
(unload-value (cons-ptr 0)
	      (list '() 3 (cons-ptr 4) 2 (cons-ptr 2) 1))]

Notice that this list could laid out in many different ways, but when
viewed through the lens of @racket[unload-value], they represent the
same list:

@ex[
(unload-value (cons-ptr 4)
	      (list (cons-ptr 2) 1 (cons-ptr 0) 2 '() 3))]	      

The idea of the interpreter that explicitly models memory will be
thread through a heap that is used to represent the memory allocated
by the program.  Operations that manipulate or create boxes and pairs
will have to be updated to work with this new representation.

So for example, the @racket[cons] operation should allocate two new
memory locations and produce a tagged pointer to the address of the
first one.  The @racket[car] operation should dereference the memory
pointed to by the given @racket[cons-ptr] value.

@ex[
(unload (alloc-cons 100 99 '()))
(unload (alloc-box 99 '()))

#;
(unload
  (match (alloc-cons 3 '() '())
    [(cons h v)
     (match (alloc-cons 2 v h)
       [(cons h v)
        (alloc-cons 1 v h)])]))]


Much of the work is handled in the new @tt{interp-prims-heap} module:

@codeblock-include["hustle/interp-prims-heap.rkt"]


@ex[
(unload (interp-prim1 'box 99 '()))
(unload (interp-prim2 'cons 100 99 '()))
(unload
  (match (interp-prim1 'box 99 '())
    [(cons h v)
     (interp-prim1 'unbox v h)]))]


Finally, we can write the overall interpreter, which threads a heap
throughout the interpretation of a program in
@racket[interp-env-heap].  The top-level @racket[interp] function,
which is intended to be equivalent to the original @racket[interp]
function that modelled memory implicitly, calls
@racket[interp-env-heap] with an initially empty heap and the unloads
the final answer from the result:

@codeblock-include["hustle/interp-heap.rkt"]



@;{ Really deserves a "bit" level interpreter to bring this idea across. }


@;codeblock-include["hustle/interp.rkt"]

@section{Representing @this-lang values}

The first thing do is make another distinction in the kind of values
in our language.  Up until now, each value could be represented in a
register.  We now call such values @bold{immediate} values.

We introduce a new category of values which are @bold{pointer} values.
We will (for now) have two types of pointer values: boxes and pairs.

So we now have a kind of hierarchy of values:

@verbatim{
- values
  + pointers (non-zero in last 3 bits)
    * boxes
    * pairs
  + immediates (zero in last three bits)
    * integers
    * characters
    * booleans
    * ...
}

We will represent this hierarchy by shifting all the immediates over 3
bits and using the lower 3 bits to tag things as either being
immediate (tagged @code[#:lang "racket"]{#b000}) or a box or pair.
To recover an immediate value, we just shift back to the right 3 bits.

The pointer types will be tagged in the lowest three bits.  A box
value is tagged @code[#:lang "racket"]{#b001} and a pair is tagged
@code[#:lang "racket"]{#b010}.  The remaining 61 bits will hold a
pointer, i.e. an integer denoting an address in memory.

The idea is that the values contained within a box or pair will be
located in memory at this address.  If the pointer is a box pointer,
reading 64 bits from that location in memory will produce the boxed
value.  If the pointer is a pair pointer, reading the first 64 bits
from that location in memory will produce one of the value in the pair
and reading the next 64 bits will produce the other.  In other words,
constructors allocate and initialize memory.  Projections dereference
memory.

The representation of pointers will follow a slightly different scheme
than that used for immediates.  Let's first talk a bit about memory
and addresses.

A memory location is represented (of course, it's all we have!) as a
number.  The number refers to some address in memory.  On an x86
machine, memory is @bold{byte-addressable}, which means each address
refers to a 1-byte (8-bit) segment of memory.  If you have an address
and you add 1 to it, you are refering to memory starting 8-bits from the
original address.

We will make a simplifying assumption and always store things in
memory in multiples of 64-bit chunks.  So to go from one memory
address to the next @bold{word} of memory, we need to add 8 (1-byte
times 8 = 64 bits) to the address.

What is 8 in binary?  @code[#:lang "racket"]{#b1000}

What's nice about this is that if we start from a memory location that
is ``word-aligned,'' i.e. it ends in @code[#:lang "racket"]{#b000},
then every 64-bit index also ends in @code[#:lang "racket"]{#b000}.

What this means is that @emph{every} address we'd like to represent
has @code[#:lang "racket"]{#b000} in its least signficant bits.  We
can therefore freely uses these three bits to tag the type of the
pointer @emph{without needing to shift the address around}.  If we
have a box pointer, we can simply zero out the box type tag to obtain
the address of the boxes content.  Likewise with pairs.


We use a register, @racket['rbx], to hold the address of the next free
memory location in memory.  To allocate memory, we simply increment
the content of @racket['rbx] by a multiple of 8.  To initialize the
memory, we just write into the memory at that location.  To construct a
pair or box value, we just tag the unused bits of the address.


The generated code will have to coordinate with the run-time system to
initialize @racket['rbx] appropriately, which we discuss in
@secref["hustle-run-time"].

So for example the following creates a box containing the value 7:

@#reader scribble/comment-reader
(racketblock
(seq (Mov 'rax (value->bits 7))
     (Mov (Offset 'rbx 0) 'rax) ; write '7' into address held by rbx
     (Mov 'rax 'rbx)            ; copy pointer into return register
     (Or 'rax type-box)         ; tag pointer as a box
     (Add 'rbx 8))              ; advance rbx one word
)

If @racket['rax] holds a box value, we can ``unbox'' it by erasing the
box tag, leaving just the address of the box contents, then
dereferencing the memory:

@#reader scribble/comment-reader
(racketblock
(seq (Xor 'rax type-box)         ; erase the box tag
     (Mov 'rax (Offset 'rax 0))) ; load memory into rax
)

Pairs are similar, only they are represented as tagged pointers to two
words of memory.  Suppose we want to make @racket[(cons 3 4)]:

@#reader scribble/comment-reader
(racketblock
(seq (Mov 'rax (value->bits 4))
     (Mov (Offset 'rbx 0) 'rax) ; write '4' into address held by rbx
     (Mov 'rax (value->bits 3))
     (Mov (Offset 'rbx 8) 'rax) ; write '3' into word after address held by rbx
     (Mov 'rax rbx)             ; copy pointer into return register
     (Or 'rax type-cons)        ; tag pointer as a pair
     (Add 'rbx 16))             ; advance rbx 2 words
)

This code writes two words of memory and leaves a tagged pointer in
@racket['rax].  It's worth noting that we chose to write the
@racket[cdr] of the pair into the @emph{first} word of memory and the
@racket[car] into the @emph{second}.  This may seem like a strange
choice, but how we lay out the memory is in some sense an arbitrary
choice, so long as all our pair operations respect this layout.  We
could have just as easily done the @racket[car] first and @racket[cdr]
second.  The reason for laying out pairs as we did will make things
slightly more convenient when implementing the @racket[cons] primitive
as we'll see later.


If @racket['rax] holds a pair value, we can project out the elements
by erasing the pair tag, leaving just the address of the pair contents,
then dereferencing either the first or second word of memory:

@#reader scribble/comment-reader
(racketblock
(seq (Xor 'rax type-cons)         ; erase the pair tag
     (Mov 'rax (Offset 'rax 8))   ; load car into rax
     (Mov 'rax (Offset 'rax 0)))  ; or... load cdr into rax
)

From here, writing the compiler for @racket[box], @racket[unbox],
@racket[cons], @racket[car], and @racket[cdr] is just a matter of
putting together pieces we've already seen such as evaluating multiple
subexpressions and type tag checking before doing projections.

@section{A Compiler for @this-lang}

The compiler for @this-lang is essentially the same as for Fraud, although
now with support for the new primitives: @racket[box], @racket[unbox],
@racket[box?], @racket[cons], @racket[car], @racket[car],
@racket[cdr], @racket[cons?], and @racket[empty?]:

@codeblock-include["hustle/compile-ops.rkt"]

We can now confirm that the compiler generates code similar to what we
wrote by hand above:

@ex[
(define (show e c)
  (compile-e (parse e) c))

(show '(box 7) '())
]

This moves the encoding of @racket[7] into @racket['rax], then writes
it into the memory address pointed to by @racket['rbx], i.e. the next
free memory location.  That address is then moved to @racket['rax] and
tagged as a box, which is the result of the expression.  The final
step is to increment @racket['rbx] by @racket[8] to advance the free
memory pointer since one word of memory is now used.

Suppose we have a box value bound to variable @racket[x], then this
code will unbox the value:

@ex[
(show '(unbox x) '(x))
]

This loads @racket[x] from the stack into @racket['rax], then does tag
checking to make sure it's a box pointer, after which it erases the
tag to reveal the address and loads that memory address into
@racket['rax], thereby retrieving the value in the box.

The way that @racket[cons], @racket[car], and @racket[cdr] work are
essentially the same, except that pairs hold two values instead of
one:

@ex[
(show '(cons 7 5) '())
(show '(car x) '(x))
(show '(cdr x) '(x))
]

We can now see why we chose to layout pairs with the @racket[cdr]
first and @racket[car] second.  Since @racket[cons] is a binary
operation, the expression which produces the @racket[car] value will
be evaluated first and pushed on the stack.  Then the expression that
produces the @racket[cdr] value will execute with its result sitting
in @racket[rax].  So at this point it's easiest to write out the
@racket[cdr] since it's already sitting in a register.  Once we do
that, we can pop the @racket[car] value into @racket['rax] and write
that.  Hence our choice for the layout.

@section[#:tag "hustle-run-time"]{A Run-Time for @this-lang}

First, we extend our runtime system's view of values to include
pointers and use C @tt{struct} to represent them:

@filebox-include[fancy-c hustle "values.h"]

The implementation of @tt{val_typeof} is extended to handle
pointer types:

@filebox-include[fancy-c hustle "values.c"]

The rest of the run-time system for @this-lang is more involved for two
main reasons:

The first is that the compiler relies on a pointer to free memory
residing in @racket['rbx].  The run-time system will be responsible
for allocating this memory and initializing the @racket['rdi]
register.  To allocate memory, it uses @tt{malloc}.  It passes the
pointer returned by @tt{malloc} to the @tt{entry} function.  The
protocol for calling functions in C says that the first argument will
be passed in the @racket['rdi] register.  Since @tt{malloc} produces
16-byte aligned addresses on 64-bit machines, @racket['rdi] is
initialized with an address that ends in @code[#:lang
"racket"]{#b000}, satisfying our assumption about addresses.

Once the runtime system has provided the heap address in
@racket['rdi], it becomes our responsibility to keep track of that
value. Because @racket['rdi] is used to pass arguments to C functions,
we can't keep our heap pointer in @racket['rdi] and expect it to be
saved. This leaves us with two options:

@itemlist[
 @item{We can ensure that we save @racket['rdi] somewhere safe whenever we
       might call a C function}

 @item{We can move the value away from @racket['rdi] as soon as possible and
       never have to worry about @racket['rdi] being clobbered during a call
       to a C function (as long as we pick a good place!)}
]

We've decided to use the second option, which leaves the choice of @emph{where}
to move the value once we receive it from the runtime system. As usual, we will
consult the System V Calling Convention, which tells us that @racket['rbx] is a
@emph{callee save} register, which means that any C function we might call is
responsible for ensuring that the value in the register is saved and restored.
In other words: we, the caller, don't have to worry about it! Because of this
we're going to use @racket['rbx] to store our heap pointer. You can see
that we do this in the compiler with @racket[(Mov 'rbx 'rdi)] as part
of our entry code.

@filebox-include[fancy-c hustle "main.c"]

The second complication comes from printing.  Now that values include
inductively defined data, the printer must recursively traverse these
values to print them.  It also must account for the wrinkle of how the
printing of proper and improper lists is different:

@filebox-include[fancy-c hustle "print.c"]

@section{Correctness}

The statement of correctness for the @|this-lang| compiler is the same
as the previous one:

@bold{Compiler Correctness}: @emph{For all @racket[e] @math{∈}
@tt{ClosedExpr}, @racket[i], @racket[o] @math{∈} @tt{String}, and @racket[v]
@math{∈} @tt{Value}, if @racket[(interp/io e i)] equals @racket[(cons
v o)], then @racket[(exec/io e i)] equals
@racket[(cons v o)].}
