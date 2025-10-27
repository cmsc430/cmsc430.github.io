#lang scribble/manual

@(require (for-label (except-in racket ... compile) a86/ast))
@(require redex/pict
	  racket/runtime-path
	  scribble/examples
	  hoax/types
	  ; (except-in "../../langs/hustle/semantics.rkt" ext lookup)
	  ; (prefix-in sem: (only-in "../../langs/hustle/semantics.rkt" ext lookup))
	  "../fancyverb.rkt"
	  "utils.rkt"
	  "ev.rkt"
          "diagrams.rkt"
	  "../utils.rkt")

@(define codeblock-include (make-codeblock-include #'h))

@(ev '(require rackunit a86 hoax hoax/compile-ops hoax/assert))
     
@(define this-lang "Hoax")

@title[#:tag this-lang]{@|this-lang|: vectors and strings}

@src-code[this-lang]

@emph{Stupidity, outrage, vanity, cruelty, iniquity, bad faith,
falsehood - we fail to see the whole array when it is facing in the
same direction as we.}

@table-of-contents[]

@section{Array data}

In the @bold{@this-lang} language, we will add two @bold{array
data types}: vectors and strings.

Vectors are fixed-length arrays of values with constant-time access
and update of the vector slots, which are numbered from @racket[0] to
one less than the number of slots in the vector.

Strings are fixed-length arrays of characters with constant-time
access and update of the character slots, which are numbered from
@racket[0] to one less than the number of slots in the string.

The new vector operations include the constructor @racket[(make-vector
_e1 _e2)], predicate @racket[(vector? _e0)], accessor
@racket[(vector-ref _e0 _e1)] and mutator @racket[(vector-set! _e0 _e1
_e2)].

These features will operate like their Racket counterparts:
@ex[
(make-vector 3 #t)
(vector? (make-vector 3 #t))
(vector-ref (make-vector 3 #t) 0)
(vector-ref (make-vector 3 #t) 2)
(let ((v (make-vector 3 #t)))
  (begin (vector-set! v 1 #f)
	 v))
]

The new string operations include the constructor @racket[(make-string
_e1 _e2)], predicate @racket[(string? _e0)], and accessor
@racket[(string-ref _e0 _e1)].  We will also add support for string
literals.

These features will operate like their Racket counterparts:
@ex[
(make-string 3 #\t)
(string? (make-string 3 #\t))
(string-ref "abc" 0)
(string-ref "abc" 2)
]



We can model this syntax as an AST data type:

@filebox-include-fake[codeblock "hoax/ast.rkt"]{
#lang racket
;; type Expr = ...
;;           | (Prim3 Op3 Expr Expr Expr)
;; type Op1 = ...
;;          | 'vector? | 'string?
;; type Op2 = ...
;;          | 'make-vector | 'vector-ref
;;          | 'make-string | 'string-ref
;; type Op3 = 'vector-set!
}

@section{Meaning of @this-lang programs, implicitly}

We extend our definition of values, representing vectors with vectors
and strings with strings (what a surprise!):

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
;; | (vector Value ...)
;; | (string Character ...)
)

The @this-lang interpreter is essentially the same as for Hustle,
although with the addition of ternary primitives, plus an extension of
the @racket[interp-prim] module:

@codeblock-include["hoax/interp-prim.rkt"]

Vectors are easy to model in the interpreter because we can rely on
vectors in the meta-level of Racket.

This of course doesn't illuminate much about these operations.  We
could, as we did for Hustle, develop an interpeter with an explicit
account of memory.  Instead, let's just jump into the details of the
compiler.

@section{Representing @this-lang values}

Now that were are comfortable with heap-allocated data-structures like
boxes and pairs, handling vectors is not too difficult.  Vectors are
similarly heap allocated.  This will require a new kind of pointer value:

@itemlist[
@item{Values
 @itemlist[@item{pointers (non-zero in last 3 bits)
             @itemlist[@item{Boxes} @item{Pairs} @item{Vectors} @item{Strings}]}
	   @item{immediates (zero in last three bits)
	     @itemlist[@item{integers} @item{characters} @item{booleans} @item{...}]}]}]

We will follow exactly the same scheme we followed for box and pairs:
vectors and strings will be uniquely tagged in the lowest three bits
and the remaining bits will indicate an address in memory which can be
obtained by zeroing out the tag bits.


@;{

@section{Detour: static data}

Before looking at how we can implement the vector and string
operations, let's take a quick side quest to talk about @bold{static
memory}.  It will be useful in making examples and it's going to help
with a small issue concerning the empty vector and empty string.  It
will also be useful in the future when we get to adding compound
literals to our language.

Static data is memory that is allocated before a program is run.  The
mechanism for allocating this data is to actually use space @emph{in}
the program to hold the data.  The program after all, like everything,
is just a blob of bits.  When it's executed, the operating system
loads the program from a file into memory.  We can use parts of that
memory at run-time if we'd like.  Typically that memory contains
executable instructions, but also we make space in the program to hold
arbitrary data.  The way to do this is to create a @bold{data section}
by using the @racket[(Data)] psuedo-instruction.  It's not actually an
instruction, rather it instructs the assembler that the subsequent
content of the program should be part of the data section in the
object file.  Within the @racket[Data] section, we can use the
@racket[Dq] ``instruction'' for defining an 8-byte quantity that is
placed directly in the file.

Here's an example where we make a @emph{static} pair.  Notice that the
program does not allocate any memory at run-time (no @tt{malloc} or
@racket[rbx] register used), yet it manages to return a pair to
the run-time system:

@ex[
(run
  (prog
    (Global 'entry)
    (Label 'entry)
    (Lea rax (Mem 'p type-cons))
    (Ret)
    (Data)
    (Label 'p)
    (Dq (value->bits #t))
    (Dq (value->bits #f))))]

It does this by embedding the pair itself into the text of the program
and giving the location of this data a name with the label
@racket['p].  When it's executed, we construct a tagged pointer into
the data section and return that.

From the run-time's perspective there's no difference between this and
pair that's allocated in the heap.  It's just a tagged pointer to two
words of memory.

We're able to use this trick because we knew ahead of time that we
want to represent @racket['(1 . 2)].  In general we can't know ahead
of time what memory a program needs to allocate or what data will be
stored in that memory, so we still need dynamically allocated memory,
but this is a useful mechanism to know about.

We can of course represent other kinds of data using this trick.
Here's a static vector @racket['#(1 2 3)]:

@ex[
(run
  (prog
    (Global 'entry)
    (Label 'entry)
    (Lea rax (Mem 'v type-vect))
    (Ret)
    (Data)
    (Label 'v)
    (Dq (value->bits 3))
    (Dq (value->bits 1))
    (Dq (value->bits 2))
    (Dq (value->bits 3))))]

}

@section{Representing and operating on vectors}

Vectors are sized @bold{heterogenous arrays} of values.  The memory
that is pointed to by a vector tagged pointer will contain the length
of the vector followed by that many words of memory, one for each
element of the vector. (Strings will be similar, with a slight twist,
which we'll examine later.)

So for example the following creates a vector of size 3 containing the
values @racket[1], @racket[#t], @racket[#\c]:

@#reader scribble/comment-reader
(ex
 (run
  (prog
    (Global 'entry)
    (Label 'entry)
    ;; Set up heap pointer from run-time system
    (Push rbx)
    (Mov rbx rdi)
    ;; Create vector
    (Mov rax (value->bits 3))
    (Mov (Mem rbx 0) rax)      ; write vector length 3
    (Mov rax (value->bits 1))
    (Mov (Mem rbx 8) rax)      ; write value 1
    (Mov rax (value->bits #t))
    (Mov (Mem rbx 16) rax)     ; write value #t
    (Mov rax (value->bits #\c))
    (Mov (Mem rbx 24) rax)     ; write value #\c
    (Mov rax rbx)
    (Xor rax type-vect)        ; tag pointer as vector
    (Add rbx 32)               ; acct for memory used
    ;; Restore registers and return
    (Pop rbx)
    (Ret))))

This creates a vector in memory like this:

@make-heap-diagram['((vect 0) 3 1 #t #\c)]

In order to explore things interactively, let's set up a little
function for running code in the context of this vector.  It will take
given instructions and run them after @racket['#(1 #t #\c)]
is in @racket[rax]:


@#reader scribble/comment-reader
(ex
 (define (eg is)
   (run
    (prog
      (Global 'entry)
      (Extern 'raise_error)
      (Label 'entry)      
      ;; Set up heap pointer from run-time system
      (Push rbx)
      (Mov rbx rdi)
      ;; Create vector
      (Mov rax (value->bits 3))
      (Mov (Mem rbx 0) rax)      ; write vector length 3
      (Mov rax (value->bits 1))
      (Mov (Mem rbx 8) rax)      ; write value 1
      (Mov rax (value->bits #t))
      (Mov (Mem rbx 16) rax)     ; write value #t
      (Mov rax (value->bits #\c))
      (Mov (Mem rbx 24) rax)     ; write value #\c
      (Mov rax rbx)
      (Xor rax type-vect)        ; tag pointer as vector
      (Add rbx 32)               ; acct for memory used

      ;; include given instructions
      is 

      ;; Restore registers and return
      (Pop rbx)
      (Ret)
      (Label 'err)
      (Call 'raise_error)))))

If we call @racket[eg] with no instructions, we just get the vector
itself back:

@ex[(eg (seq))]

OK, now let's think about some of the operations we want to implement,
starting with @racket[vector-length].  Since the vector stores its
length as the first word in memory, implementing
@racket[vector-length] amounts to fetching that word of memory:

@ex[(eg (seq (Mov rax (Mem rax (- 0 type-vect)))))]

The operation should also do some type tag checking to make sure its
argument actually is a vector first.  We can define a helper function
for asserting the ``vectorness'' of something in a given register:

@ex[(assert-vector rax)]

Of course for our example, the assertion succeeds and we still get its
length:

@ex[
 (eg (seq (assert-vector rax)
          (Mov rax (Mem rax (- 0 type-vect)))))]


This will be our implementation of @racket[vector-length], now let's
do @racket[vector-ref].  If @racket[rax] contains a vector, we can
access the element at index @racket[_i] by dereferencing the memory at
location @racket[(- (add1 (* _i 8)) type-vect)].  The @racket[add1] is
needed to account for the word at the start that holds the length.  So
for example, to get the three elements of our example vector:

@ex[
(eg (seq (Mov rax (Mem rax (- 8 type-vect)))))
(eg (seq (Mov rax (Mem rax (- 16 type-vect)))))
(eg (seq (Mov rax (Mem rax (- 24 type-vect)))))]

This is the essence of @racket[vector-ref], but there's a bit more to
the story.  First, we don't actually know what index were fetching
until run-time.  Because @racket[vector-ref] is a binary operation and
the order of the arguments is the vector, then the index, we will have
the vector value on the stack and index value in @racket[rax].  We can
simulate this in our example by pushing the vector and writing our
index into @racket[rax].  We then need instructions to (1) pop the
stack into a temporary register and (2) deference the memory using the
@racket[rax] instead of a literal offset:

@#reader scribble/comment-reader
(ex
 (eg (seq (Push rax)
          (Mov rax 0) ; index = 0
          ;; Start of vector-ref code
          (Pop r8)
          (Mov rax (Mem r8 rax (- 8 type-vect))))))

This example is a little deceiving because the index we happened to
want is @racket[0], but remember that the argument to
@racket[vector-ref] will be a value, so we really should have set
@racket[rax] to @racket[(value->bits 0)].  We got lucky here because
the value @racket[0] is represented by the bits @racket[0].  But let's
think about an index of @racket[1].  If @racket[rax] holds the
@emph{value} @racket[1], then that will be the bits
@racket[#,(value->bits 1)].  If @racket[rax] holds the value
@racket[2], then that will be the bits @racket[#,(value->bits 2)].
But those aren't the right offsets that we want; we want offsets
@racket[8] and @racket[16] respectively.  In general, and index value
@racket[_i] will be represented by the bits @racket[(* _i 16)], but
want offset @racket[(* _i 8)].  The simplest way to convert from the
value @racket[_i] to the appropriate offset is to shift to the right
1 bit.  So here's how can access all the elements uniformly, starting
from an index value in @racket[rax]:

@#reader scribble/comment-reader
(ex
 (eg (seq (Push rax)
          (Mov rax (value->bits 0)) ; index = 0
          ;; Start of vector-ref code	 
          (Pop r8)
	  (Sar rax 1) ; convert int to byte offset
          (Mov rax (Mem r8 rax (- 8 type-vect)))))
 (eg (seq (Push rax)
          (Mov rax (value->bits 1)) ; index = 1
          ;; Start of vector-ref code	 
          (Pop r8)
	  (Sar rax 1) ; convert int to byte offset
          (Mov rax (Mem r8 rax (- 8 type-vect)))))
 (eg (seq (Push rax)
          (Mov rax (value->bits 2)) ; index = 2
          ;; Start of vector-ref code	 
          (Pop r8)
	  (Sar rax 1) ; convert int to byte offset
          (Mov rax (Mem r8 rax (- 8 type-vect))))))

We're getting closer.  We now just need to do a little more error checking.
First, we should check that @racket[rax] holds a non-negative integer
and that the stack argument is a vector.  Here we have a little helper
for asserting that something is a natural number:

@ex[(assert-natural rax)]

So we can add in the type checking like this:

@#reader scribble/comment-reader
(ex
 (eg (seq (Push rax)
          (Mov rax (value->bits 2)) ; index = 2
          ;; Start of vector-ref code	  
          (Pop r8)
	  (assert-natural rax)
	  (assert-vector r8)
	  (Sar rax 1) ; convert int to byte offset
          (Mov rax (Mem r8 rax (- 8 type-vect))))))

Finally, we have just a little more checking to do.  We need to be
sure to signal an error if you go off the end of a vector, which our
code currently does not do:

@#reader scribble/comment-reader
(ex
 (eg (seq (Push rax)
          (Mov rax (value->bits 7)) ; index = 7
          ;; Start of vector-ref code	  
          (Pop r8)
	  (assert-natural rax)
	  (assert-vector r8)
	  (Sar rax 1) ; convert int to byte offset
          (Mov rax (Mem r8 rax (- 8 type-vect))))))

Here we read past the end of the vector by using an index of
@racket[7] on a vector of length @racket[3].  That should be an error,
otherwise it's possible to access arbitrary memory.  To do this, we
need to fecth the length and make sure the index value is less than
the length value.  Now when we use an index of @racket[7], we get an
error, but for a valid index, we get the appropriate element:

@#reader scribble/comment-reader
(ex
 (eg (seq (Push rax)
          (Mov rax (value->bits 7)) ; index = 7
          ;; Start of vector-ref code
          (Pop r8)
	  (assert-natural rax)
	  (assert-vector r8)
	  (Mov r9 (Mem r8 (- type-vect)))
	  (Cmp rax r9)
	  (Jge 'err)
	  (Sar rax 1)
          (Mov rax (Mem r8 rax (- 8 type-vect)))))
 (eg (seq (Push rax)
          (Mov rax (value->bits 2)) ; index = 2
          ;; Start of vector-ref code
          (Pop r8)
	  (assert-natural rax)
	  (assert-vector r8)
	  (Mov r9 (Mem r8 (- type-vect)))
	  (Cmp rax r9)
	  (Jge 'err)
	  (Sar rax 1)
          (Mov rax (Mem r8 rax (- 8 type-vect))))))	  

We've got @racket[vector-ref] down.  Let's turn to
@racket[vector-set!]. At its core, we're just doing a memory write to
replace a vector element with a new one.  For example, here's how we
can update the second element to be @racket[#f]:

@ex[
 (eg (seq (Mov r8 (value->bits #f))
          (Mov (Mem rax (- 16 type-vect)) r8)))]

But of course there's more to it than this.  Just like
@racket[vector-ref] we need to do some type- and bounds-checking.  We
also have to deal with the fact that this is a ternary primitive---it
takes 3 arguments.  The first two will be on the stack and the third
will be in @racket[rax].  The first argument is the vector, the second
is in the index, and the third is the element to write into the
vector.  We can get pretty close with this:

@#reader scribble/comment-reader
(ex
 (eg (seq (Push rax)
          (Mov rax (value->bits 2)) ; index = 2
	  (Push rax)
	  (Mov rax (value->bits #f))
          ;; Start of vector-set! code
	  (Mov r10 rax) ; update value
          (Pop rax)     ; index
	  (Pop r8)      ; vector
	  (assert-natural rax)
	  (assert-vector r8)
	  (Mov r9 (Mem r8 (- type-vect)))
	  (Cmp rax r9)
	  (Jge 'err)
	  (Sar rax 1)
          (Mov (Mem r8 rax (- 8 type-vect)) r10))))

Although it's impossible to verify from the result, this actually
@emph{has} the intended effect of modifying the vector.  It's been
written to mimic the code for @racket[vector-ref] by placing the
vector in @racket[r8] and the index in @racket[rax].  The value
that's going to be written gets moved over to another temporary
register, @racket[r10].  The type- and bounds-checking is exactly as
in @racket[vector-ref].  The only change is that the last instruction
is a write instead of a read.

But why did we get @racket[1] in the end?  Well that's just what
happens to be in @racket[rax].  We had the index value there, then we
shifted to the right to convert it to a byte offset.  That happens be
interpreted as the value @racket[1].  What value should we have ended
up with?  The answer that Racket goes with is @racket[(void)], which
signifies that the expression is evaluated for effect.  That's easy
enough, we can just move @racket[(void)] into @racket[rax] at the very
end:

@#reader scribble/comment-reader
(ex
 (eg (seq (Push rax)
          (Mov rax (value->bits 2)) ; index = 2
	  (Push rax)
	  (Mov rax (value->bits #f))
          ;; Start of vector-set! code
	  (Mov r10 rax) ; update value
          (Pop rax)     ; index
	  (Pop r8)      ; vector
	  (assert-natural rax)
	  (assert-vector r8)
	  (Mov r9 (Mem r8 (- type-vect)))
	  (Cmp rax r9)
	  (Jge 'err)
	  (Sar rax 1)
          (Mov (Mem r8 rax (- 8 type-vect)) r10)
	  (Mov rax (value->bits (void))))))

Notice that it appears nothing is returned, but this is just the
REPL's handling of @racket[(void)]: it's printed form is empty.

This is the correct code for @racket[vector-set!], but how do we see
that it did the right thing to the vector?  How do we confirm that it
had the intended effect?  Well in this example, we lost our handle on
the vector.  We can adapt the example to save a way a copy of the
vector and then restore it at the end in order to observe the change
happened.  Here we do that by saving the vector on the stack (twice:
once to restore later and once as an argument to
@racket[vector-set!]):

@#reader scribble/comment-reader
(ex
 (eg (seq (Push rax) ; save vector
          (Push rax)
          (Mov rax (value->bits 2)) ; index = 2
	  (Push rax)
	  (Mov rax (value->bits #f))
          ;; Start of vector-set! code
	  (Mov r10 rax) ; update value
          (Pop rax)     ; index
	  (Pop r8)      ; vector
	  (assert-natural rax)
	  (assert-vector r8)
	  (Mov r9 (Mem r8 (- type-vect)))
	  (Cmp rax r9)
	  (Jge 'err)
	  (Sar rax 1)
          (Mov (Mem r8 rax (- 8 type-vect)) r10)
	  (Mov rax (value->bits (void)))
	  ;; End of vector-set! code
	  ;; Restore to see modification
	  (Pop rax))))


As you can see, the element at index @racket[2] has changed to
@racket[#f].

So we've now done @racket[vector-length], @racket[vector-ref],
@racket[vector-set!].  What about @racket[make-vector]?

The @racket[make-vector] operation takes two arguments: the length of
the vector to construct and a value used to initialize each element of
the vector.  Note that neither of these values are known at
compile-time, in general.

Now if we punt on initialization, making a vector is pretty easy:

@#reader scribble/comment-reader
(ex
 (eg (seq (Mov rax (value->bits 3)) ; length argument
          (Push rax)
	  (Mov rax (value->bits #t)) ; init argument
	  ;; Start of make-vector code
	  (Pop r8)
	  (assert-natural r8)
	  (Mov (Mem rbx 0) r8) ; write length
	  (Mov rax rbx)
	  (Xor rax type-vect)  ; create tagged pointer
	  (Add rbx 8)          ; acct for stored length
	  (Sar r8 1)           ; convert to bytes, acct for elements
	  (Add rbx r8)))
	  )

This code checks the type of the length argument to make sure its
valid, then writes the length to memory, constructs a tagged pointer
to that memory, and then adjusts the heap pointer based on the length
of the vector.  But you'll notice the vector shows up as containing
@racket[0]s.  That's just an artifact of our heap being zeroed out to
start, which is not guaranteed and we could have any possible bits for
elements in the vector, including bits that do not encode any value,
or worse, bits that appear to encode tagged pointer value, but point
to invalid memory locations.  So we really need to initialize the
elements to avoid this undefined and dangerous behavior.

To do that, we need to generate a run-time @emph{loop}.  The loop
needs to cycle through as many times as the length argument, writing
the initialization value each time to the appropriate location in
memory. We can't avoid a loop because we can't (in general) know what
the length argument is going to be until run-time.


@#reader scribble/comment-reader
(ex
 (eg (seq (Mov rax (value->bits 3)) ; length argument
          (Push rax)
	  (Mov rax (value->bits #t)) ; init argument
	  ;; Start of make-vector code
	  (Pop r8)
	  (assert-natural r8)

	  (Mov (Mem rbx 0) r8) ; write length
	  (Sar r8 1)	       ; convert to bytes
	  (Mov r9 r8)          ; save for heap adjustment

          ;; start initialization	  
	  (Label 'loop)
	  (Cmp r8 0)
	  (Je 'done)
	  (Mov (Mem rbx r8) rax)
	  (Sub r8 8)
	  (Jmp 'loop)
	  (Label 'done)
          ;; end initialization

	  (Mov rax rbx)
	  (Xor rax type-vect)  ; create tagged pointer
	  (Add rbx r9)         ; acct for elements and stored length
	  (Add rbx 8))))


Now this code successfully does the initialization and everything
seems good... but there is a corner case where this code could be
improved.  Consider the case of @racket[(make-vector 0 #t)].  This
code will dutiful create an empty vector, represented in memory like
this:

@make-heap-diagram['((vect 0) 0)]

It uses up one word of memory to store the length, which is
@racket[0].  That doesn't seem so bad, but consider a program like

@racketblock[
(begin (make-vector 0 #t)
       (make-vector 0 #f))]


This will create @emph{two} empty vectors, using up two words of
memory:

@make-heap-diagram['((vect 1) 0 0)]

Of course, both of the empty vectors are identical; every empty vector
is identical: its a tagged pointer to a single word containing
@racket[0].  It seems silly to use memory at all since we kind of know
everything there is to know about the empty vector already.  Moreover,
we usually think of (and talk about) @emph{the} empty vector, rather
than @emph{an} empty vector, because really having just a single empty
vector is all you need.

How can we take this idea and incorporate it into our design.  First,
let's see how we can make a single, canonical representation of the
empty vector.  We will do this by using a @bold{data section} to
@bold{statically allocate} memory for representing our canonical empty
vector.

@ex[
(eg (seq (Data)
         (Label 'empty)
         (Dq 0)
         (Text)
         (Lea rax (Mem 'empty type-vect))))]


This is our first encounter with a data section, so let's dig in a
little.  The @racket[Data] declaration tells the assembler that
instructions that follow should live in the ``data'' part of the
program, rather than the ``text'' part that contains executable
instructions.  Within the data section, we use the @racket[Dq]
``psuedo-instruction'' to specify exactly the bits we want placed in
the object file; @racket[Dq] is saying we want to use 64-bits to hold
@racket[0].  We have a label, which works just any other label: it
gives a symbolic name to a location in the file.

What's happening is we are using space @emph{in the program} to hold
some data.  We can reference and use that memory just like any other
memory.  The space we created consists of a single word holding
@racket[0], which is found at the location named @racket['empty].
After we set up our data section, we switch back to @racket[Text] mode
and continue with executable instructions.  In this case, we use
@racket[Lea] to load the address of the @racket['empty] location,
adjusted with the @racket[type-vect] tag.  In other words, we have a
vector-tagged pointer to a word of memory containing @racket[0].  In
other words, @racket[rax] contains the empty vector.

Now you'll notice this program didn't touch the heap at all.  So we
have made an empty vector without using up valuable heap space.  What
remains is to adapt @racket[make-vector] so that if it's asked to
construct an empty vector it returns this canonical empty vector
rather than constructing a new one.  Let's add a special case the code
above.  We'll assume the canonical empty vector is statically
allocated with the label @racket['empty] somewhere (it doesn't matter
where as long as it's somewhere in the program).

@#reader scribble/comment-reader
(ex
 (eg (seq (Data)
          (Label 'empty) ; canonical empty vector memory
	  (Dq 0)
          (Text)
          (Mov rax (value->bits 0)) ; length argument
          (Push rax)
	  (Mov rax (value->bits #t)) ; init argument
	  ;; Start of make-vector code
	  (Pop r8)
	  (assert-natural r8)

	  ; special case for length = 0
	  (Cmp r8 0)
	  (Jne 'nonzero)
	  ; return canonical representation
	  (Lea rax (Mem 'empty type-vect))
	  (Jmp 'theend)

	  ;; Code for nonzero case
          (Label 'nonzero)
	  (Mov (Mem rbx 0) r8) ; write length
	  (Sar r8 1)	       ; convert to bytes
	  (Mov r9 r8)          ; save for heap adjustment

	  ;; start initialization
	  (Label 'loop)
	  (Cmp r8 0)
	  (Je 'done)
	  (Mov (Mem rbx r8) rax)
	  (Sub r8 8)
	  (Jmp 'loop)
	  (Label 'done)
          ;; end initialization

	  (Mov rax rbx)
	  (Xor rax type-vect)  ; create tagged pointer
	  (Add rbx r9)         ; acct for elements and stored length
	  (Add rbx 8)
	  (Label 'theend))))

Here we have added a simple test at the beginning to determine if the
length argument is @racket[0].  If it is, we construct a
tagged-pointer to the canonical representation of the empty vector and
jump to the end of the instructions.  If the length is non-zero, the
code jumps to the instructions we had before that does dynamic
allocation with initialization.


One final improvement we can make is to rework our loop knowing that
@racket[r8] will never be @racket[0] to start, shaving off a
comparison and resulting a single conditional jump at the end of the
loop:

@#reader scribble/comment-reader
(ex
 (eg (seq (Data)
          (Label 'empty) ; canonical empty vector memory
	  (Dq 0)
          (Text)
          (Mov rax (value->bits 3)) ; length argument
          (Push rax)
	  (Mov rax (value->bits #t)) ; init argument
	  ;; Start of make-vector code
	  (Pop r8)
	  (assert-natural r8)

	  ; special case for length = 0
	  (Cmp r8 0)
	  (Jne 'nonzero)
	  ; return canonical representation
	  (Lea rax (Mem 'empty type-vect))
	  (Jmp 'theend)

	  ;; Code for nonzero case
          (Label 'nonzero)
	  (Mov (Mem rbx 0) r8) ; write length
	  (Sar r8 1)	       ; convert to bytes
	  (Mov r9 r8)          ; save for heap adjustment
	  
          ;; start initialization	  	  	 
	  (Label 'loop)
	  (Mov (Mem rbx r8) rax)
	  (Sub r8 8)
	  (Cmp r8 0)
	  (Jne 'loop)
          ;; end initialization

	  (Mov rax rbx)
	  (Xor rax type-vect)  ; create tagged pointer
	  (Add rbx r9)         ; acct for elements and stored length
	  (Add rbx 8)
	  (Label 'theend))))

And now we have essentially arrived at the code for
@racket[make-vector].  In the compiler we should generate the label
names we use, but otherwise, this is the code for
@racket[make-vector].  The canonical representation of the empty
vector can be included in the output of the top-level @racket[compile]
function.

We are now ready to turn to strings.


@section{Representing and operating on strings}

Strings will be very much like vectors---after all, they are just
another kind of array value.  The key difference is that strings are
arrays not of arbitrary values, but specifically of characters.
The fact that strings are @bold{homogenous arrays} of characters
will be useful in specializing the memory representation we use.


While we could use a vector to represent a string, with a unique
pointer tag, this would waste memory: every character would be
allocated 64-bits of memory.  Since we use unicode codepoints to
represent characters and because strings are @bold{homogenous} we need
at most 21-bits to represent each character of a string.

There are many different representations for strings of Unicode
characters, but one of the simplest is that of UTF-32.  It is a
fixed-width encoding that uses 32-bits for each character.  This is
still wasteful, but has the benefit of supporting @racket[string-ref]
in constant time.  Had we not needed to implement @racket[string-ref]
with this guarantee, other less wasteful encodings such as UTF-8 could
be used.  We'll use UTF-32 as a nice balance of simplicity and economy
of memory usage.

So the basic idea will be that a string will be represented by a
distinct tag in the lower three bits of an 8-byte aligned address.
The pointer points to memory where, like a vector, the first word
holds the length of the string, followed by an array of 32-bit slots,
each holding a character codepoint.

There is a wrinkle: an odd length string would seemingly occupy a
segment of memory that does not fall on an 8-byte boundary.  For
example, a string of length 1 would occupy 64+32=96-bits=12-bytes of
memory.  This would violate our assumption that the next free memory
address ends in @code[#:lang "racket"]{#b000}.

The solution is simple: allocate 32-bits more when the length is odd.
This sacrifices a small amount of memory in order to preserve the
invariant that allows our low-order tagging of pointers.

Another complication is that we will now want to read and write
32-bits of memory.  Until now, we've always operated on memory in
units of 64-bits.  We could ``fake it'' by reading and writing 64-bits
at a time, carefully making sure to ignore or preserve half of the
bits, however this makes the code a mess and is inefficient.

The better solution is to use a 32-bit register: @racket[eax].  The
@racket[eax] register is not actually a new register, but rather is a
name for the lower 32-bits of @racket[rax] (so be careful: modifying
one will change the other---they are the same register!).  Whenever
@racket[eax] is used in a memory read or write, the CPU will read or
write 32-bits instead of 64.

So, suppose we want to create the string @racket["abc"]:

@#reader scribble/comment-reader
(ex
 (run
  (prog
    (Global 'entry)
    (Label 'entry)
    ;; Set up heap pointer from run-time system
    (Push rbx)
    (Mov rbx rdi)
    ;; Create string
    (Mov rax (value->bits 3))
    (Mov (Mem rbx 0) rax)      ; write string length 3
    (Mov eax (char->integer #\a))
    (Mov (Mem rbx 8) eax)      ; write codepoint for #\a
    (Mov eax (char->integer #\b))
    (Mov (Mem rbx 12) eax)     ; write codepoint for #\b
    (Mov eax (char->integer #\c))
    (Mov (Mem rbx 16) eax)     ; write codepoint for #\c
    (Mov rax rbx)
    (Xor rax type-str)         ; tag pointer as string
    (Add rbx 24)               ; acct for memory used
    ;; Restore registers and return
    (Pop rbx)
    (Ret))))

This creates a string in memory like this:

@make-heap-diagram['((str 0) "abc")]


In this diagram, we use half-sized boxes to indicate that only 4 bytes
are used per codepoint.  Also the contents of the array are not
values, but codepoints (shown as the letters the codepoint encodes).
In odd length strings, we write @racket[0] for the padded element, but
in reality there are arbitrary bits at that memory location, which is
fine because they will never be read.

At first glance, this looks remarkably similar to creating a vector,
however there are some imporant things to notice:

@itemlist[

@item{First, this code does not use @racket[value->bits] on the
characters, but rather @racket[char->integer].  In other words, the
array here is not an array of values, but rather an array of
codepoints.}

@item{Second, the @racket[eax] register is used to write the
codepoints to memory, which means that 32-bits (4-bytes) are written.}

@item{Third, the offsets are growing by @racket[4] with each write,
reflecting the fact that we're only writing @racket[4] bytes per
character.}

@item{Finally, notice that the adjustment to @racket[rbx] at the end
doesn't actually line up with how much memory was written: the code
wrote 8 bytes for the length and 3x4 bytes for the codepoints, i.e.
20 bytes, yet the heap pointer is incremented by @racket[24].  This is
to maintain our heap pointer invariant of being 8-byte aligned.  It
comes at the cost of wasting 4-bytes per odd-lengthed string, which
you are seeing here.}

]

Notice that a string like @racket["fred"] is not represented the same
as a vector of characters @racket[#(#\f #\r #\e #\d)], which uses more
space.  Compare:

@make-heap-diagram['((str 0) "fred")]

versus:

@make-heap-diagram['((vect 0) 4 #\f #\r #\e #\d)]



Now let's set things up like we did before to be able to interactively
write examples in order to arrive at the code for
@racket[string-length], @racket[string-ref], and @racket[make-string]:

@#reader scribble/comment-reader
(ex
 (define (eg is)
   (run
    (prog
      (Global 'entry)
      (Extern 'raise_error)
      (Label 'entry)      
      ;; Set up heap pointer from run-time system
      (Push rbx)
      (Mov rbx rdi)
      ;; Create string
      (Mov rax (value->bits 3))
      (Mov (Mem rbx 0) rax)      ; write string length 3
      (Mov eax (char->integer #\a))
      (Mov (Mem rbx 8) eax)      ; write codepoint for #\a
      (Mov eax (char->integer #\b))
      (Mov (Mem rbx 12) eax)     ; write codepoint for #\b
      (Mov eax (char->integer #\c))
      (Mov (Mem rbx 16) eax)     ; write codepoint for #\c
      (Mov rax rbx)
      (Xor rax type-str)         ; tag pointer as string
      (Add rbx 24)               ; acct for memory used

      ;; include given instructions
      is 

      ;; Restore registers and return
      (Pop rbx)
      (Ret)
      (Label 'err)
      (Call 'raise_error)))))


Again, doing nothing produces the example string:

@ex[(eg (seq))]

The @racket[string-length] primitive is pretty-much identical to its
vector counterpart:

@ex[(eg (seq (assert-string rax)
             (Mov rax (Mem rax (- 0 type-str)))))]


The @racket[string-ref] primitive follows the same outline as
@racket[vector-ref], except that we compute a byte offset from the
index by shifting the to right @bold{2} places instead of 1.  This is
because for an index value of @racket[_i], we want the raw bits for
@racket[(* _i 4)].  Since our encoding of integers is to multiply by
16, shifting right by 2 gives us the encoded integer times 4.  We also
use @racket[eax] to load the 32-bit codepoint from memory at the end.
Finally, unlike @racket[vector-ref], once the element of the array has
been read, we have to do a little bit of work to construct the
@emph{value} that should be returned.  Remember: we are storing
codepoints, not values, so @racket[string-ref] will need to construct
a character value from the codepoint, which it does by shifting and
tagging.

@#reader scribble/comment-reader
(ex
 (eg (seq (Push rax)
          (Mov rax (value->bits 2)) ; index = 2
          ;; Start of string-ref code
          (Pop r8)
	  (assert-natural rax)
	  (assert-string r8)
	  (Mov r9 (Mem r8 (- type-str)))
	  (Cmp rax r9)
	  (Jge 'err)
	  (Sar rax 2)
          (Mov eax (Mem r8 rax (- 8 type-str)))
	  (Sal rax char-shift)
	  (Xor rax type-char))))
	  

Similarly, @racket[make-string] will follow a similar pattern to
@racket[make-vector].  The key differences here are: the use of
@racket[eax], shifting right by @racket[2] to compute the number of
bytes for codepoints, converting the character value in @racket[rax]
to a codepoint via a right shift, and finally an alignment to
@racket[rbx] that ensures our heap invariant is maintained:

@#reader scribble/comment-reader
(ex
 (eg (seq (Data)
          (Label 'empty) ; canonical empty string memory
	  (Dq 0)
          (Text)
          (Mov rax (value->bits 4)) ; length argument
          (Push rax)
	  (Mov rax (value->bits #\a)) ; init argument
	  ;; Start of make-string code
	  (Pop r8)
	  (assert-natural r8)

	  ; special case for length = 0
	  (Cmp r8 0)
	  (Jne 'nonzero)
	  ; return canonical representation
	  (Lea rax (Mem 'empty type-str))
	  (Jmp 'theend)

	  ;; Code for nonzero case
          (Label 'nonzero)

	  (Mov (Mem rbx 0) r8) ; write length
	  (Sar r8 2)	       ; convert to bytes
	  (Mov r9 r8)          ; save for heap adjustment

	  (Sar rax char-shift) ; convert to codepoint

	  ;; start initialization
	  (Label 'loop)
	  (Mov (Mem rbx r8 4) eax)
	  (Sub r8 4)	  
	  (Cmp r8 0)
	  (Jne 'loop)
          ; end initialization

	  (Mov rax rbx)
	  (Xor rax type-str)   ; create tagged pointer
	  (Add rbx r9)         ; acct for elements and stored length
	  (Add rbx 8)
	  ;; Pad to 8-byte alignment
	  (Add rbx 4)
	  (Sar rbx 3)
	  (Sal rbx 3)
	  (Label 'theend))))

The alignment here is done by adding 4.  If @racket[rbx] is not
currently aligned, this will bring it into alignment, making the lower
3 bits zero and then the shift right and left have no impact.  On the
other hand, if @racket[rbx] is already aligned, the @racket[Add] simply sets
the third bit, which is then stripped off by the shifts.


The handling of the empty string is just like that of the empty
vector.  In fact, if you look at the data section, it's
@emph{identical} to the data section for the empty vector.  This
suggests that both the empty string and the empty vector can share the
same static memory for their canonical representation.  This doesn't
mean the empty string and the empty vector are the same value, they
are not.  But they @emph{point} to the same memory.  One points with
string tag and the other points with a vector tag!

@section{Compiling string literals}

The final issue to address is how to compile string literals such as
@racket["fred"].  For this we can turn to the mechanism of static
memory that we saw earlier with the empty vector and empty string.
Since a string literal is totally determined at compile-time, we can
avoid allocation in the heap and instead allocate in the program
itself.  For example, it's easy to construct @racket["abc"] using
static memory:

@ex[
(eg (seq (Data)
         (Label 'abc)
	 (Dq (value->bits 3))
	 (Dd (char->integer #\a))
	 (Dd (char->integer #\b))
	 (Dd (char->integer #\c))
	 (Text)
	 (Lea rax (Mem 'abc type-str))))]

Notice that this uses the @racket[Dd] psuedo-instruction for
specifying 32-bit quantities in the data section.  This lays out a
sized array of codepoints in the data section and the @racket[Lea]
instruction constructs a string-tagged pointer to it.

Using this idea we can formulate a compiler for string literals:

@ex[
(define (compile-string s)
  (let ((l (gensym 'string))
        (n (string-length s)))
    (match s
      ["" (seq (Lea rax (Mem 'empty type-str)))]
      [_
       (seq (Data)
            (Label l)
            (Dq (value->bits n))
            (compile-string-chars (string->list s))
            (if (odd? n) (Dd 0) (seq))
            (Text)
            (Lea rax (Mem l type-str)))])))
]

@section{A Compiler for @this-lang}

Most of the work for the @this-lang compiler is done in the
compilation of the new operations:

@codeblock-include["hoax/compile-ops.rkt"]

We can now confirm that the compiler generates code similar to what we
wrote by hand above:

@ex[
(define (show e c)
  (compile-e (parse e) c))

(show '(make-vector 3 #t) '())
(show '(vector-ref x 1) '(x))
(show '"abc" '())
(show '(string-ref x 1) '(x))
]


@section{A Run-Time for @this-lang}

First, we extend the value interface to include vectors:

@filebox-include[fancy-c hoax "values.h"]

The implementation of @tt{val_typeof} is extended to handle
another pointer type:

@filebox-include[fancy-c hoax "values.c"]

Printing is updated to handle vectors and strings.  Note that printing
of strings seems complicated by this code is actually auto-generated
from the Unicode specification.

@filebox-include[fancy-c hoax "print.c"]
