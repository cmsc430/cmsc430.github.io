#lang scribble/manual

@(require (for-label (except-in racket ... compile) a86/ast))
@(require redex/pict
          racket/runtime-path
          scribble/examples
	  "../fancyverb.rkt"
	  "utils.rkt"
	  "ev.rkt"
	  "../utils.rkt"
	  "diagrams.rkt"
	  hustle/types)

@(define codeblock-include (make-codeblock-include #'h))

@(ev '(require rackunit a86 hustle hustle/unload hustle/heap hustle/interp-prims-heap hustle/compile-ops))

@(define this-lang "Hustle")
@(define prefix (string-append this-lang "-"))

@title[#:tag this-lang]{@|this-lang|: heaps and lists}

@src-code[this-lang]

@emph{A little and a little, collected together, become a great deal;
the heap in the barn consists of single grains, and drop and drop
makes an inundation.}

@table-of-contents[]

@section[#:tag-prefix prefix]{Inductive data}

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

@section[#:tag-prefix prefix]{Empty lists can be all and end all}

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
;; type Datum = ... | '()
;; type Op1 = ... | 'box | 'car | 'cdr | 'unbox | 'box? | 'cons?
;; type Op2 = ... | 'cons
}

@section[#:tag-prefix prefix]{Parsing}

Mostly the parser updates for @|this-lang| are uninteresting.  We've
added some new unary and binary primitive names that the parser now
recognizes for things like @racket[cons], @racket[car],
@racket[cons?], etc., however, one wrinkle is that we now have a very
limited form of @racket[quote], so it's worth discussing what this
means for the concrete syntax of our language.

@subsection{Quote and the notion of self-quoting datums}

In Racket, some datums are @emph{self-quoting}, which means they don't
need to be quoted.  For example, @racket[5] is a self-quoting datum.
You actually can quote @racket[5]: @racket['5].  This means exactly
the same thing as @racket[5].  These are two different concrete
syntaxes for exactly the same thing: the integer literal @racket[5].

The whole reason for @racket[quote] is that it is used to indicate
when something is a datum when it would otherwise be interpreted as an
expression.  This becomes relevant we start having datums with
parentheses in them because for example the expression @racket[(add1
5)] and the datum @racket['(add1 5)] mean very different things,
whereas @racket[5] and @racket['5], do not.  So lists (and pairs) are
@emph{not} self-quoting: we are required to use @racket[quote] when we
want to write such datums: @racket[(code:quote (add1 5))], which can
also be written in shorthand form as @racket['(add1 5)].

Up until this point, all of our datums have been self-quoting and we
have thus just left @racket[quote] out of the concrete syntax.  Now we
have @racket[quote] for the empty list. 

@subsection{Parsing quoted datums and self-quoting datums}

In our parser, we introduce a predicate for identifying self-quoting
datums (@racket[self-quoting-datum?]), which includes integers,
boolean, and characters; and another for identifying the larger class
of datums (@racket[datum?]), which includes all the self-quoting
datums, plus the empty list @racket['()].  Notice in the parser that
self-quoting datums are expressions, but quoted datums must occur
withing a @racket[(code:quote ...)] form.

@ex[
(parse 5)
(code:comment "This is quoting at the Racket-level and the parser sees 5")
(parse '5) 
(code:comment "This is quoting at the Hustle-level and the parser see '(quote 5)")
(code:comment "which it parses at (Lit 5)")
(parse ''5)
(code:comment "This is quoting at the Racket-level and the parser see '(),")
(code:comment "but () is not valid expression syntax, hence a parse error")
(eval:error (parse '()))
(code:comment "This is quoting at the Hustle-level and the parser sees '(quote ()),")
(code:comment "which is parsed as the empty list.")
(parse ''())
]

It's worth noting that while we have added pairs and boxes to our
language, we have not added @emph{literal notation} for these things.
(We will eventually.)  So things like @racket['(1 . 2)] are not valid
syntax in Hustle:

@ex[
(eval:error (parse ''(1 . 2)))
]


@codeblock-include["hustle/parse.rkt"]

@section[#:tag-prefix prefix]{Meaning of @this-lang programs, implicitly}

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

@section[#:tag-prefix prefix]{Meaning of @this-lang programs, explicitly}

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

@section[#:tag-prefix prefix]{Representing @this-lang values}

Since we have grown the set of values in our langauge, we have to
address the issue of how this set of values can be represented at
run-time.  The new values we've added are: the empty lists, pairs, and
boxes.  Of these, the empty list is straightfoward: it can be
represented like any of our other enumerated values: pick an unused
bit pattern and designate it as representing the empty list.  Boxes
and pairs will require some new mechanisms.

@subsection{The need for memory}

There's an obvious conundrum encountered as soon as you start thinking
about representing Hustle values.  Remember that values have to fit in
a register, i.e. we have at most 64 bits to represent all of our
values.  We've gotten by so far by using some small number of bits to
encode the type of the value and the remaining bits to represent the
value itself.  We now have new kinds of values: pairs and boxes.
These are distinct from the existing types of values, so we will need
to devote some bits to indicating these new types.  But what about the
value part?  Well a pair contains two values, e.g. @racket[(cons 1 2)]
contains both the value @racket[1] and the value @racket[2].  How can
we possibly fit both, which each may take up 64 bits, into a single
64-bit register?  Even if we were to chop integers down to say 30 bits
so that two could fit in a word with type tag bits left over, that
only works for @emph{pairs of integers}, and even then it only works
for small integers.  You might also be tempted to use more registers
to represent values.  Perhaps we use two registers to store values.
One is unused except in the case of pairs, then each register holds
the elements of the pair.

But the true power of these new kinds of values is the ability to
construct @bold{arbitrarily large collections of data}.  That power
comes from being able to construct a pair of @emph{any} two values,
@emph{including other pairs}.  So while we might be able to find ways
of encoding small collections, we have to face the fact that we might
construct large collections and that no strategy that depends upon a
fixed number of values will suffice.


We need memory.  When creating a pair, just like in our explicit
heap-based interpreter, we need to allocate memory and think of the
pair value as having a @emph{pointer} to that memory.  In this way we
can construct arbitrarily large collections of values, bound only by
the available memory on our system.


@subsection{Tagged pointer values}

That idea that we can use pointers to memory to represent datatypes
like boxes and pairs seems simple enough, but we also still have to
deal with the other aspects of our value encoding.  Namely, we need to
be able to distinguish all of the disjoint datatypes in our language.
A pointer is seemingly just an arbitrary 64-bit integer.  How can we
tell a pointer apart from other bit patterns that represent integers,
booleans, characters, etc.?

A memory location is represented (of course, it's all we have!) as a
number.  The number refers to some address in memory.  On an x86
machine, memory is @bold{byte-addressable}, which means each address
refers to a 1-byte (8-bit) segment of memory.  If you have an address
and you add 1 to it, you are refering to memory starting 8-bits from the
original address.

It's tempting to follow the approach we've already used: shift and
tag.  In other words, take a pointer, shift it to the left some number
of bits and tag the lower bits with a unique pattern to indicate the
type as being either a pair or a box.

This worked for things like booleans, characters, eof, void,
etc. because we chould shift to the left without losing any
information.  In the case of integers, we @emph{did} lose some
information: we cut down the range of integer values that are
representable in our language.  But the integers that were left still
made sense.

Unfortunately pointers don't work that way.  If we shift a pointer and
bits fall off, we're no longer pointing at the same memory location.
So what are we to do?

There are many options, but we adopt a simple approach.  It starts
from the observation that we will always allocate memory in multiples
of 8 bytes.  So if our memory starts out aligned to 8 bytes, then all
of the addresses we will reprent will also be aligned to 8 bytes.
That means addresses will always end in @binary[#b000 3].  We can
therefore use these bits to store information @emph{without losing any
information about the address itself}!

The first thing to do is make another distinction in the kind of
values in our language.  Up until now, each value could be represented
in a register alone.  We now call such values @bold{immediate} values.

We introduce a new category of values which are @bold{tagged pointer}
values.  Tagged pointers also fit into registers, but they refer to memory
so they cannot be understood by the contents of the register alone; we have to
take the memory into consideration too.

We will (for now) have two types of tagged pointer values: boxes and
pairs.

So we now have a kind of hierarchy of values:

@itemlist[
@item{Values
  @itemlist[@item{Tagged pointers (non-zero in last three bits)
               @itemlist[@item{Boxes} @item{Pairs}]}]
  @itemlist[@item{Immediates (zero in last three bits)
               @itemlist[@item{Integers} @item{Characters} @item{Booleans} @item{...}]}]}]

We will represent this hierarchy by shifting all the immediates over
@number->string[imm-shift] bits and using the lower
@number->string[imm-shift] bits to tag things as either being
immediate (tagged @binary[0 imm-shift]) or a box or pair.  To recover
an immediate value, we just shift back to the right
@number->string[imm-shift] bits.

So for example:

@(define (val-eg v)
  @item{the value @racket[#,v] is represented by the bits @binary[(value->bits v) imm-shift], aka @racket[#,(value->bits v)].})

@itemlist[
  @val-eg[#t]
  @val-eg[#f]
  @val-eg[0]
  @val-eg[1]  
  @val-eg[5]
  @val-eg[#\a]
  @val-eg[#\b]
]

The pointer types will be tagged in the lowest
@number->string[imm-shift] bits.  A box value is tagged
@binary[type-box imm-shift] and a pair is tagged @binary[type-cons
imm-shift].  The remaining @number->string[(- 64 imm-shift)] bits will
hold a pointer, i.e. an integer denoting an address in memory.  To
obtain the address, no shifting is done; instead we simply zero-out
the tag.

The idea is that the values contained within a box or pair will be
located in memory at this address.  If the tagged pointer is a box
pointer, reading 64 bits from that location in memory will produce the
boxed value.  If the pointer is a pair pointer, reading the first 64
bits from that location in memory will produce one of the value in the
pair and reading the next 64 bits will produce the other.  In other
words, constructors allocate and initialize memory.  Projections
dereference memory.

It's more difficult to construct examples of tagged pointer values
because a value's representation now depends on what's in memory.
Moveover, a value's representation is no longer unique.  We can no
longer say things like ``the value @racket['(1 . 2)] is represented by
the bits...'' because there are many possible bits that could
represent this value.

We can however say that if a memory address holds two consecutive
values: 1 and 2, then a value @racket['(1 . 2)] may be represented by
the bits you get when you tag that pointer as a pair.  Let's use this
idea to write some representation examples.  These are all stated
hypothetically based on what has to be in memory:

@itemlist[
@item{if address 0 holds the value @racket[#t] and address 8 holds the value @racket[#f],
 then the value @racket['(#t . #f)] may be represented by the bits @binary[type-cons],
 aka @racket[#,type-cons].}]

This a perfectly valid, if somewhat unrealistic example.  Your
operating system is likely not going to let you use address 0 (or 8
for that matter).  That's OK.  We don't really care what the actual
address is @emph{so long as it's always divisible by 8}.  That's the
only thing our encoding scheme depends upon.

Let's try another example:

@itemlist[
@item{if address 98760 holds the value @racket[#t] and address 98768 holds the
 value @racket[#f], then the value @racket['(#t . #f)] may be represented by the
 bits @binary[(+ 98760 type-cons)], aka @racket[#,(+ 98760 type-cons)].}]

A couple things to notice in this example: (1) the address is
divisible by 8, (2) the representation of the value @racket['(#t
. #f)] @emph{is not}.  That's because we tacked on the cons type tag
in unused bits of the pointer.  We haven't lost any information
though: to recover the pointer, simply erase the tag (either by
or-ing, xor-ing, or subtracting the tag from the tagged pointer; they
are all equivalent if the address is divisible by 8 and the tag is
less than 8, which it is).

So in general, we have:

@itemlist[

@item{if address @racket[_a] is divisible by 8 and holds the value
 @racket[_v₁] and address @racket[_a] + 8 holds the value @racket[_v₂],
 then the value @racket[(cons _v₁ _v₂)] may be represented by the bits
 @racket[(bitwise-xor _a #,(binary type-cons))].}

@item{if address @racket[_a] is divisible by 8 and holds the value
 @racket[_v],
 then the value @racket[(box _v)] may be represented by the bits
 @racket[(bitwise-xor _a #,(binary type-box))].}

]

@(define ra (+ (random 0 10000) 123456))

We can also turn things around:

@itemlist[ @item{if bits @binary[(+ ra type-cons)] (aka @racket[#,(+
ra type-cons)]) represents a value, then at address @binary[ra] (aka
@racket[#,ra]) there is some value @racket[_v₁] and at @binary[(+ ra
8)] (aka @racket[#,(+ ra 8)]) there is some value @racket[_v₂].}]

We know this because the bits end in the pair type tag, thus the value
represented is a pair and it must be the case that there are two
values at the address encoded in the bits.

In general:

@itemlist[ @item{if bits @racket[_b] represents a value and @racket[(=
(bitwise-and _b #,(binary ptr-mask)) #,(binary type-cons))], then at
address @racket[(bitwise-xor _b #,(binary type-cons))] there is some
value @racket[_v₁] and at address @racket[(+ (bitwise-xor _b #,(binary
type-cons)) 8)] there is some value @racket[_v₂].}
@item{if bits @racket[_b] represents a value and @racket[(=
(bitwise-and _b #,(binary ptr-mask)) #,(binary type-box))], then at
address @racket[(bitwise-xor _b #,(binary type-box))] there is some
value @racket[_v].}]


OK, we now have a good model of how these new kinds of values
can be @emph{represented}, but how can we actually construct
and manipulate them?


@subsection{A source of free memory}

We've established how we can use memory to represent boxes and
pairs, but it remains to be seen where this memory comes from and how
we can use it to construct these kinds of values.

So far, our only ability to allocate memory has come from using the
stack.  When we push variable bindings or the results of intermediate
computations on the stack, we ``allocate'' memory by using more of the
stack space.  When we pop these values off, we ``deallocate'' that
memory by making it available to be overwritten by future stack
pushes.

Since it seems to be the only game in town, it's obviously tempting to
use the stack to allocate pairs and boxes.  But here's the rub:
variable bindings and intermediate results follow a straightforward
stack discipline that we can read off from the text of a program.  For
example, in @racket[(let ((_x _e₁)) _e₂)], we know that we can push
@racket[_e₁]'s value on the stack before executing the code for
@racket[_e₂] and then pop it off at the end of the instructions for
the whole @racket[let].  Likewise in @racket[(+ _e₁ _e₂)], we can push
@racket[_e₁]'s value on the stack while computing @racket[_e₂] and
then pop it off to do the addition.  But with @racket[(cons _e₁ _e₂)],
if pushed the values of @racket[_e₁] and @racket[_e₂] on the stack and
then made a tagged pointer to that value, @emph{when} would we pop it
off?  Definitely not at the end of the @racket[(cons _e₁ _e₂)]
expression because after all we need to be able to access the parts of
the pair after making it; deallocating then would construct and
immediate destroy the pair.  On the other hand, @emph{not} popping at
the end of the expression destroys one of our compiler invariants
which is that, by the time we get to the end of the instructions for
the compiled code of an expression, we have restored the stack to
whatever state it was in at the start.  If we destroy that, how will
variables and binary operations work?  Notice the shape of the stack
could no longer be read off from the text of a program.  Consider:

@racketblock[
(let ((x (if (zero? (read-byte)) (cons 1 2) #f)))
  x)
]

Where is @racket[x]'s value on the stack?  If we allocate pairs on the
stack, there may or may not be a pair sitting before it @emph{and
there's no way to know at compile-time}.  Good luck compiling that
variable occurrence!

So... the stack is out.  Mostly this is because the lifetime of
pointer values is not lexical: it's not a property of the text of a
program, but rather its execution.  So we will need another source
of free memory; memory that can outlive elements on the stack.
We'll call this memory the @bold{heap}.

For this, we will turn to our run-time system.  Before it calls the
compiled code of a program, we will have it allocate a chunk of memory
and pass it as an argument to the compiled code.  The compiled code
will then install that pointer into a designated register, much like
how @racket[rsp] is designated to hold the stack pointer.  So instead
of doing this in the @tt{main} entry point of the run-time:

@verbatim|{
  val_t result;
  result = entry();
  print_result(result);  
}|

We'll update it to:

@verbatim|{
  heap = malloc(8 * heap_size); // allocate heap
  val_t result;
  result = entry(heap);         // pass in pointer to heap
  print_result(result);  
}|

Here we are allocating some number of words of memory (how many words
is given by the constant @tt{heap_size}), via @tt{malloc} and then
calling the compiled code with an argument which is the pointer to
this freshly allocated memory.  The compiled code can hold on to a
pointer to this memory and write into it in to allocate new pairs and
boxes.

We will designate the @racket[rbx] register as the heap pointer
register.  This is an arbitrary choice, other than the fact that we
selected a callee-saved (aka non-volatile) register.  This is useful
for us because we need the heap pointer to be preserved across calls
into the run-time system.  Had we designated a caller-save (volatile)
register, we'd need to save and restore it ourselves before and after
@emph{every} call.  Choosing a non-volatile register does mean we have
to save and restore the @emph{caller's} @racket[rbx], but we do this
just once at the beginning and end of the program.

So our top-level compiler looks like this:

@#reader scribble/comment-reader
(racketblock
;; ClosedExpr -> Asm
(define (compile e)
  (prog (Global 'entry)
        (Label 'entry)
	...
        (Push rbx)        ; save the caller's register
        (Mov rbx rdi)     ; install heap pointer
        (compile-e e '()) ; run!
        (Pop rbx)         ; restore caller's register
	...
        (Ret))))


Now @racket[compile-e] can produce code that uses the heap pointer in
register @racket[rbx].  If we want to use some of this memory we can
write into it, adjust @racket[rbx] by the amount we just used, and
then produce tagged pointers to the location we wrote to.

So for example the following creates a box containing the value 7:

@#reader scribble/comment-reader
(racketblock
(seq (Mov rax (value->bits 7))
     (Mov (Mem rbx 0) rax)     ; write '7' into address held by rbx
     (Mov rax rbx)             ; copy pointer into return register
     (Xor rax type-box)        ; tag pointer as a box
     (Add rbx 8))              ; advance rbx one word
)




If @racket[rax] holds a box value, we can ``unbox'' it by erasing the
box tag, leaving just the address of the box contents, then
dereferencing the memory:

@#reader scribble/comment-reader
(racketblock
(seq (Xor rax type-box)     ; erase the box tag
     (Mov rax (Mem rax 0))) ; load memory into rax
)

As a slight optimization, instead of doing a run-time tag erasure, we
can simply adjust the offset by the tag quantity so that reading the
contents of a box (or any other pointer value) is a single
instruction:

@#reader scribble/comment-reader
(racketblock
(seq (Mov rax (Mem rax (- type-box)))) ; load memory into rax
)

Pairs are similar, only they are represented as tagged pointers to two
words of memory.  Suppose we want to make @racket[(cons #t #f)]:

@#reader scribble/comment-reader
(racketblock
(seq (Mov rax (value->bits #t))
     (Mov (Mem rbx 0) rax)      ; write '#t' into address held by rbx
     (Mov rax (value->bits #f))
     (Mov (Mem rbx 8) rax)      ; write '#f' into word after address held by rbx
     (Mov rax rbx)              ; copy pointer into return register
     (Xor rax type-cons)        ; tag pointer as a pair
     (Add rbx 16))              ; advance rbx 2 words
)

This code writes two words of memory and leaves a tagged pointer in
@racket[rax].

If @racket[rax] holds a pair value, we can project out the elements by
erasing the pair tag (or adjusting our offset appropriately) and
dereferencing either the first or second word of memory:

@#reader scribble/comment-reader
(racketblock
(seq (Mov rax (Mem rax (- 0 type-cons)))   ; load car into rax
     (Mov rax (Mem rax (- 8 type-cons))))  ; or... load cdr into rax
)

From here, writing the compiler for @racket[box], @racket[unbox],
@racket[cons], @racket[car], and @racket[cdr] is just a matter of
putting together pieces we've already seen such as evaluating multiple
subexpressions and type tag checking before doing projections.

@subsection{Making examples}

It's more challenging to use @racket[asm-interp] to actually execute
examples since we need some coordination between the run-time system
and @racket[asm-interp] in order to allocate the heap, but for the
moment, let's see how we can effectively work around this coordination
to make examples that actually run without using the run-time system.


An alternative to asking the run-time system to allocate memory (which
in turn asks our operating system to allocate memory), we can instead
bake some memory into the text of our assembly program itself.  This
ends up as space @emph{in the object file} of our compiled and
assembled program that also holds the instructions for our code.  To
do this we can create a @bold{data section} in our code.  Up until now
our assembly programs have lived in the @bold{text section}, which is
the part that holds instructions to be executed.  In contrast, the
data section just holds data, not instructions.  When the operating
system runs our program, it loads the object file into memory, so
anything we put into the data section (as well as all of our
instructions) are in memory and we can use this memory.  Fundamentally
it's no different from the memory allocated by @tt{malloc} in our
run-time system.  The key difference is that this space comes from the
object file and therefore has to be determine at compile-time rather
than at run-time using @tt{malloc}.  Hence it is referred to as
@bold{static memory}.

When constructing an assembly program, we can switch the data section
by using the @racket[(Data)] psuedo-instruction.  What this means is
that the instructions that follow should be assembled into the data
part of the file and not the text (code) part.  To switch back to the
text section, use the @racket[(Text)] directive.  Within the data
section we can use the @racket[Dq] ``instruction'' to designate one
(64-bit) word of static memory.  It's not actually an instruction
(hence the scare-quotes) because it doesn't execute; instead it says
put these bits at this spot in the program.  So this sequence:

@racketblock[
(seq (Data)
     (Dq 1)
     (Dq 2)
     (Dq 3))]

is saying that in the data section there should be three words of
memory containing the bits @racket[1], @racket[2], and @racket[3],
respectively.

If we want to get a pointer to this memory, we need to name the
location with a label and then use the @racket[Lea] instruction to
load it's address into a register at run-time:


@racketblock[
(seq (Data)
     (Label 'd)
     (Dq 1)
     (Dq 2)
     (Dq 3)
     (Text)
     (Lea rax 'd))]

Let's try it out:

@ex[
(asm-interp
  (prog (Global 'entry)
        (Label 'entry)
	(Data)
	(Label 'd)
	(Dq 1)
	(Dq 2)
	(Dq 3)
	(Text)
	(Lea rax 'd)
	(Ret)))]

Now, what we get back is the @emph{address} of that memory.  It's some
arbitrary number (but notice what it is divisible by!).

If we'd like we can dereference that memory to fetch the contents:

@ex[
(asm-interp
  (prog (Global 'entry)
        (Label 'entry)
	(Data)
	(Label 'd)
	(Dq 1)
	(Dq 2)
	(Dq 3)
	(Text)
	(Lea rax 'd)
	(Mov rax (Mem rax 0))
	(Ret)))]


@;{

If we changed the offset to @racket[8], we'd get @racket[2];
@racket[16] would get @racket[3].  We have essentially created a
little static array.  We can also write into that array:

@ex[
(asm-interp
  (prog (Global 'entry)
        (Label 'entry)
	(Data)
	(Label 'd)
	(Dq 1)
	(Dq 2)
	(Dq 3)
	(Text)
	(Lea rax 'd)
	(Mov r8 100)
	(Mov (Mem rax 0) r8)
	(Mov rax (Mem rax 0))
	(Ret)))]

So we can use this as a basis for making little executable examples to
run our compiler.  The idea is we can @emph{statically} allocate heap
space and then use that to execute code.

Here's a little helper function for (statically) allocating a given
number of words and loading a pointer to it into a given register:

@#reader scribble/comment-reader
(ex
;; Statically allocate i words of memory and
;; set register r to its address
;; Reg Nat -> Asm
(define (alloc r i)
  (let ((l (gensym 'data)))
    (seq (Data)
         (Label l)
         (make-list i (Dq 0))
         (Text)
         (Lea r (Mem l)))))
)

}

To do that, let's just call @tt{malloc} ourselves!

@ex[
(asm-interp
  (prog (Global 'entry)
  	(Extern 'malloc)
        (Label 'entry)
	(Sub rsp 8)
	(Mov rdi (* 10 8))
	(Call 'malloc)
	(Add rsp 8)
	(Ret)))]

This sets up a call to @tt{malloc(8*10)}, which allocates 10 words and
returns the pointer in @racket[rax].

OK, let's make a pair:

@#reader scribble/comment-reader
(ex
(eval:alts
  (asm-interp
    (prog (Global 'entry)
    	  (Extern 'malloc)
          (Label 'entry)
          (Push rbx)
	  (Mov rdi (* 10 8))
	  (Call 'malloc)
	  (Mov rbx rax)
          (Mov rax (value->bits #t))
          (Mov (Mem rbx 0) rax)      ; write #t
          (Mov rax (value->bits #f))
          (Mov (Mem rbx 8) rax)      ; write #f
          (Mov rax rbx)              ; copy pointer
          (Xor rax type-cons)        ; tag as pair
          (Add rbx 16)               ; account for memory used
	  (Pop rbx)
          (Ret)))
  (begin
    (define this
      (asm-interp
        (prog (Global 'entry)
	      (Extern 'malloc)
              (Label 'entry)
              (Push rbx)	      
              (Mov rdi (* 10 8))
              (Call 'malloc)
	      (Mov rbx rax)
              (Mov rax (value->bits #t))
              (Mov (Mem rbx 0) rax)      ; write #t
              (Mov rax (value->bits #f))
              (Mov (Mem rbx 8) rax)      ; write #f
              (Mov rax rbx)              ; copy pointer
              (Xor rax type-cons)        ; tag as pair
              (Add rbx 16)               ; account for memory used
              (Pop rbx)
              (Ret))))
     this)))


This @emph{should} create a pair that is represented in memory like
this:

@make-heap-diagram['((cons 0) #t #f)]

What we get is @racket[#,(ev 'this)], which doesn't @emph{look} like a
pair.  But remember, @racket[asm-interp] is just giving us back
whatever is in the @racket[rax] register after calling this code: it's
giving us back @emph{bits}, not @emph{values}.  But!  You should
notice that these bits are encoding a pair value.  If we look at the
three least significant bits, we see @binary[type-cons 3], aka
@racket[#,type-cons]:

@ex[(eval:alts (bitwise-and #,(ev 'this) #,(binary ptr-mask))
               (bitwise-and this ptr-mask))]

That tells us that @racket[#t] and @racket[#f] live at memory
addresses @racket[(bitwise-xor #,(ev 'this) #,(binary type-cons 3))]
and @racket[(+ (bitwise-xor #,(ev 'this) #,(binary type-cons 3)) 8)],
respectively.

@margin-note{This is not actually true.  Using Racket's
@racketmodname[ffi/unsafe] library provides a way to cast integers to
pointers and dereference arbitrary memory.  In fact,
@racketmodname[ffi/unsafe] is the thing that makes @racket[asm-interp]
possible. The memory safety guarantee only applies to programs that
safely use @racketmodname[ffi/unsafe], which is easiest to do by not
using it all!}

Now, how can we fetch them?  On the Racket side of things, we just
have an integer and an integer is not a pointer in Racket.  By design,
the language does not give you a way to cast an arbitrary integer to
some kind of pointer datatype that can be dereferenced.
 This is important to
guarantee memory safety.  Of course, @racket[asm-interp] offers a huge
back-door to that safety, so we can whip up our own operation to
dereference whatever memory address we'd like:

@#reader scribble/comment-reader
(ex
;; Integer -> Integer
;; Fetch the word at given address
(define (mem-ref ptr)
  (asm-interp
    (prog (Global 'entry)
          (Label 'entry)
	  (Mov r8 ptr)
          (Mov rax (Mem r8))
          (Ret))))

(eval:alts (mem-ref (bitwise-xor #,(ev 'this) type-cons))
	   (mem-ref (bitwise-xor this type-cons)))
(eval:alts (mem-ref (+ (bitwise-xor #,(ev 'this) type-cons) 8))
           (mem-ref (+ (bitwise-xor this type-cons) 8)))
)

And while these also don't look like @racket[#t] and @racket[#f],
remember:

@ex[(value->bits #t)
    (value->bits #f)]


Aside: We should be very careful with this operation; you can do bad
things with it:

@ex[(eval:alts (mem-ref #,(hex #xDEADBEEF))
	       (eval:error (mem-ref #xDEADBEEF)))]

We're now in a position to actually reconstruct the pair value on
the Racket side of things:

@#reader scribble/comment-reader
(ex
;; Bits -> (cons Value Value)
;; Constructs a pair from bits encoding a pair value
(define (cons-bits->cons b)
  (cons (bits->value (mem-ref (+ b (- 0 type-cons))))
        (bits->value (mem-ref (+ b (- 8 type-cons))))))

(eval:alts (cons-bits->cons #,(ev 'this))
	   (cons-bits->cons this))
)


And this works great so long as the values in the pair aren't
themselves tagged pointers, which of course, they could be!  What
should we do in that case?  Well, figure out what kind of pointer they
are, dereference their contents and construct the appropriate kind of
Racket value (either a box or a pair).  We can do this recursively to
complete convert from whatever encoding of a value we get back into
the corresponding Racket value.  In other words, extending
@racket[bits->value] to work in the presence of tagged pointer values
involves just the kind of thing we've written:

@codeblock-include["hustle/types.rkt"]

You'll notice that instead of the @racket[mem-ref] we wrote, it uses
Racket's own ``unsafe'' operations.  The only difference is that this
is more efficient, bypassing the overhead of @racket[asm-interp].

With @racket[bits->value] in place, we can now build up some utilities
for running programs with the run-time system linked in and using
@racket[bits->value] to construct the result value:

@codeblock-include["hustle/run.rkt"]

Let's make the list @racket['(1 2 3)].  Remember that @racket['(1 2
3)] is just @racket[(cons 1 (cons 2 (cons 3 '())))].

@#reader scribble/comment-reader
(ex
(run
  (prog (Global 'entry)
        (Label 'entry)
        (Push rbx)
        (Mov rbx rdi)
        (Mov rax (value->bits 1))
        (Mov (Mem rbx 0) rax)
        (Mov rax rbx)
        (Add rax (+ 16 type-cons))
        (Mov (Mem rbx 8) rax)
        (Mov rax (value->bits 2))
        (Mov (Mem rbx 16) rax)
        (Mov rax rbx)
        (Add rax (+ 32 type-cons))
        (Mov (Mem rbx 24) rax)          
        (Mov (Mem rbx 24) rax)
        (Mov rax (value->bits 3))
        (Mov (Mem rbx 32) rax)
        (Mov rax (value->bits '()))
        (Mov (Mem rbx 40) rax)          
        (Mov rax rbx)   
        (Xor rax type-cons)
        (Add rbx (* 8 6)) ; account for 6 words used
        (Pop rbx)
        (Ret))))


These instructions create a list that is laid out in the heap like
this:

@make-heap-diagram[
 '((cons 0)
  1
  (cons 2)
  2
  (cons 4)
  3
  '())]

@margin-note{See if you can construct the list this way.}
Of course there are many ways to make the same list.  We could, for
example, write instructions that made exactly the same list but
laid out like this:

@make-heap-diagram[
 '((cons 4)
  3
  '()  
  2
  (cons 0)
  1
  (cons 2))]

Both of these would result in the same value from the perspective of
@racket[bits->value].

Now that we can make examples and have a good idea of how to write
instructions to create boxes and pairs in memory, let's write the
compiler.


@section[#:tag-prefix prefix]{A Compiler for @this-lang}

There aren't any new expression forms in @this-lang; all of the work
is done in the implementation of the new primitives.  Predicates like
@racket[box?], @racket[cons?], and @racket[empty?] are simple:
@racket[box?] and @racket[cons?] mask the pointer tag bits and compare
against the appropriate tag; @racket[empty?] tests whether the bits
are equal the bits for @racket['()].

For @racket[box], we know the argument to the @racket[box] constructor
will be in @racket[rax] register and we need to emit code that will:
write that value into memory at the current heap pointer location,
move and tag a pointer to that memory into @racket[rax], and finally
increment @racket[rbx] to account for the memory used:

@ex[
(compile-op1 'box)
(exec (parse '(box 10)))]


This creates a box value in memory that looks like this:

@make-heap-diagram['((box 0) 10)]


To @racket[unbox] a box value, again we have the argument in the
@racket[rax] register.  We must check that the argument actually is a
box by checking its tag, signalling a run-time type error if its not.
If that succeeds, we can dereference the memory by reading the memory
location pointed to by the tagged pointer.  When dereferencing, we
account for the tag by subtracting it as an offset.

@ex[
(compile-op1 'unbox)
(exec (parse '(unbox (box 10))))]

For @racket[cons], which is a binary operator, we know the first
argument will be the first element of the stack and that the second
argument will be in the @racket[rax] register.  The compiler emits
code that pops the argument from the stack, writes both to memory,
creates a tagged pointer to the memory in @racket[rax], and increments
@racket[rbx] by @racket[16] to account for the two words of memory
used.

@ex[
(compile-op2 'cons)
(exec (parse '(cons 1 2)))]

In order to avoid using a temporary register, this code writes the
@emph{second} argument first, but at offset @racket[8], then pops into
@racket[rax], writing the first argument second at offset @racket[0].
It copies @racket[rbx] into @racket[rax], tags it a pair, then
increments @racket[rbx] appropriately.  It creates a pair in memory
that looks like this:

@make-heap-diagram['((cons 0) 1 2)]

Accessing the parts of a pair is similar to @racket[unbox]: it checks
the type, then reads the memory address at the appropriate offset.

@ex[
(compile-op1 'car)
(compile-op1 'cdr)
(exec (parse '(car (cons 1 2))))
(exec (parse '(cdr (cons 1 2))))]


Notice here that in @racket[car], the offset is @racket[#,(- 0
type-cons)], which is @racket[(- 0 type-cons)], while in @racket[cdr]
it is @racket[#,(- 8 type-cons)], which is @racket[(- 8 type-cons)].

We now have all the pieces to make lists or nested lists.

@ex[
(exec (parse '(cons 1 (cons 2 (cons 3 '())))))]



Putting it all together we get the compiler for the new primitives:
@racket[box], @racket[unbox], @racket[box?], @racket[cons],
@racket[car], @racket[car], @racket[cdr], @racket[cons?], and
@racket[empty?]:

@codeblock-include["hustle/compile-ops.rkt"]




@section[#:tag "hustle-run-time"]{A Run-Time for @this-lang}


Our compiler relies on the fact that @racket[rbx] points to available
memory, but where did this memory come from?  Well that will be the
job of the run-time system: before it runs the code our compiler
generated, it will ask the operating system to allocate a block of
memory and then pass its address as an argument to the @racket[entry]
function the compiler emits.


To allocate memory, it uses @tt{malloc}.  It passes the pointer
returned by @tt{malloc} to the @tt{entry} function.  The protocol for
calling functions in the System V ABI says that the first argument
will be passed in the @racket[rdi] register.  Since @tt{malloc}
produces 16-byte aligned addresses on 64-bit machines, @racket[rdi] is
initialized with an address that ends in @code[#:lang
"racket"]{#b000}, satisfying our assumption about addresses.

Once the runtime system has provided the heap address in
@racket[rdi], it becomes our responsibility to keep track of that
value. Because @racket[rdi] is used to pass arguments to C functions,
we can't keep our heap pointer in @racket[rdi] and expect it to be
saved. This leaves us with two options:

@itemlist[
 @item{We can ensure that we save @racket[rdi] somewhere safe whenever we
       might call an external function}

 @item{We can move the value away from @racket[rdi] as soon as possible and
       never have to worry about @racket[rdi] being clobbered during a call
       to a C function (as long as we pick a good place!)}
]

We decided to use the second option, which leaves the choice of
@emph{where} to move the value once we receive it from the runtime
system. As usual, we will consult the System V Calling Convention,
which tells us that @racket[rbx] is a @emph{callee save} register,
which means that any external function we might call is responsible
for ensuring that the value in the register is saved and restored.  In
other words: we, the caller, don't have to worry about it! Because of
this we're going to use @racket[rbx] to store our heap pointer. You
can see that we do this in the compiler with @racket[(Mov rbx rdi)] as
part of our entry code.

@filebox-include[fancy-c hustle "main.c"]


@subsection{Updating the run-time system's notion of Values}

We extend our runtime system's view of values to include
pointers and use C @tt{struct} to represent them:

@filebox-include[fancy-c hustle "values.h"]

The implementation of @tt{val_typeof} is extended to handle
pointer types:

@filebox-include[fancy-c hustle "values.c"]


@subsection{Printing Values}

Now that values include inductively defined data, the printer must
recursively traverse these values to print them (this is exactly
analogous to how @racket[bits->value] had to recursively construct
values, too).  It also must account for the wrinkle of how the
printing of proper and improper lists is different:

@filebox-include[fancy-c hustle "print.c"]

@section[#:tag-prefix prefix]{Correctness}


The statement of correctness for the @|this-lang| compiler is the same
as the previous one, although it is worth noting that it's use of
@racket[bits->value] within @racket[exec/io] is hiding some subtleties
since it recursively constructs the result value.

@margin-note{FIXME: should this be defined in terms of Answers?}

@bold{Compiler Correctness}: @emph{For all @racket[e] @math{∈}
@tt{ClosedExpr}, @racket[i], @racket[o] @math{∈} @tt{String}, and @racket[a]
@math{∈} @tt{Answer}, if @racket[(interp/io e i)] equals @racket[(cons
a o)], then @racket[(exec/io e i)] equals
@racket[(cons a o)].}
