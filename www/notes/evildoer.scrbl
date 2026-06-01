#lang scribble/manual

@(require (for-label (except-in racket ... compile ->) a86/printer a86/ast a86/interp ffi/unsafe))
@(require redex/pict
	  racket/runtime-path
	  scribble/examples
	  evildoer/runtime/types
	  "../fancyverb.rkt"
	  "utils.rkt"
	  "ev.rkt"
	  "../utils.rkt")

@(define (shellbox . s)
   (parameterize ([current-directory (build-path langs "evildoer")])
     (filebox (emph "shell")
	      (fancyverbatim "fish" (apply shell s)))))


@(define codeblock-include (make-codeblock-include #'h))

@(ev '(require rackunit a86 evildoer evildoer/correct evildoer/compiler/compile-ops evildoer/compiler/compile evildoer/executor/host))
@;{This is needed for the example that uses current-objects}
@(ev `(current-directory ,(path->string (build-path langs "evildoer"))))

@(require (for-syntax racket/base "utils.rkt"))
@(begin-for-syntax
   (define gcd.c
     #<<HERE
#include <inttypes.h>
int gcd(int64_t n1, int64_t n2) {
    return (n2 == 0) ? n1 : gcd(n2, n1 % n2);
}
HERE
     )

  (parameterize ([current-directory (build-path langs "evildoer")])
    (save-file "gcd.c" gcd.c)))

@title[#:tag "Evildoer"]{Evildoer: change the world a couple nibbles at a time}

@src-code["evildoer"]

@emph{Warning: Side effects may include itching, burning,
 oozing, weeping. Not intended for heart patients and those
 with nervous disorders.}

@table-of-contents[]

@section{Reading and writing bytes}

So far, the languages we've consider have had the following
property: the result of evaluation is determined entirely as
a function of the expression being evaluated. Which is to
say, the meaning of a program is determined entirely by the
text of the program. This is a nice property in that it
makes reasoning about programs pretty clear cut: if you know
the program, you can know exactly what it computes. However,
many real-world programs (like the very compiler we are
writing!) do not have this property. Instead they interact
with the outside world and compute results based on the
state of the world.

For example, consider the @tt{compile-stdin.rkt} program,
which reads the contents of stdin and compiles
it. The meaning of this program depends on the state of input
port. Similarly, it prints out assembly
code to the standard output port. So not only does this
program depend on the outside world, it changes it too.

Let's design a language that has a simple mechanism for
interacting with the outside world. It will be able to read
and write a byte of information at a time (i.e. an integer
between 0 and 256) from the standard input port and output
port, respectively.

We'll call it @bold{Evildoer}.

To the syntax of expressions, we add the following operations:

@itemlist[
 @item{@racket[write-byte] @tt|{: Byte -> Void}|: writes given byte to stdout, produces nothing.}
 @item{@racket[read-byte] @tt|{: -> Byte or EOF}|: reads a byte from stdin, if there is one, EOF otherwise.}
 @item{@racket[peek-byte] @tt|{: -> Byte or EOF}|: peeks a byte from stdin, if there is one, EOF otherwise.}
 ]

These operations will behave like their Racket counterparts.

To complement these operations, we add two new values:

@itemlist[
  @item{void: a value used to indicate something has been done for effect only and has no useful result.}
  @item{eof: a value used to indicate the end of an input port has been reached.}
 ]

The void value arises as the result of an expression that is evaluated
for effect only, such as @racket[write-byte].  The eof value arises as
the result of reading the end of a port.  So that these values can be
accessed more directly, we'll also add:

@itemlist[
  @item{@racket[eof] @tt{: EOF} bound to the end-of-file value, and}
  @item{@racket[void] @tt{: -> Void} a function that produces the void value.}
]


In order to recognize the end-of-file value, we add the following predicate (just as in Racket):

@itemlist[
 @item{@racket[eof-object?] @tt|{: Any -> Boolean}|: determines if argument is the eof value.}
]

Finally, we add a simple sequencing construct to first
evaluate an expression for effect and then evaluate another
expression for its result.

@itemlist[
 @item{@racket[(begin _e0 _e1)]: evaluates @racket[_e0], then @racket[_e1].}
]


Abstract syntax and parsing is done as you would expect.
Since we now have primitive operations that take 0 arguments,
we split the @racket[Prim] constructor into @racket[Prim0]
and @racket[Prim1].

@codeblock-include["evildoer/syntax/ast.rkt"]

The s-expression parser is defined as follows:

@codeblock-include["evildoer/syntax/parse.rkt"]

@ex[
(parse 'eof)
(parse '(void))
(parse '(read-byte))
(parse '(peek-byte))
(parse '(write-byte 97))
(parse '(eof-object? eof))
(parse '(begin (write-byte 97)
	       (write-byte 98)))]

@section{Reading and writing bytes in Racket}


Racket has a Byte data type that is @emph{not} disjoint from
other datatypes; it's simply an integer in @math{(0,256]}.
The operations @racket[read-byte] and @racket[write-byte]
read and write, respectively, a byte to or from stdin or
stdout (by default).

Let's look at an example of @racket[write-byte]:

@ex[
(write-byte 97)
(write-byte 109)
]

A byte, when written corresponds to an ASCII character,
which is why you see @tt{a} for 97 and @tt{m} for 109.

A subtle, but crucial point here that these expressions are
@emph{printing}, i.e. writing bytes to stdout. But they
don't @emph{produce} any value. Or more precisely, they
print and then produce a value that indicates "no useful
value has been produced." In OCaml, this value is called
"unit"; in Racket, it's called "void." When the REPL is
given an expression that produces void as its result, it
doesn't show anything. Here's any example that uses the
Racket @racket[void] function, which simply returns the void
value:

@ex[
(void)
(+ 2 3)
(void)
(void)
"fred"
]

It's important to note that void is just a value like any
other value; it's not literally "nothing." It's just handled
specially by the REPL in this case by not showing anything.
Were we to put the void value within another value, the REPL
shows it:

@ex[
 (define xs (list (void) (void) 3 (void)))
 xs
 (length xs)
 (first xs)
 (void? (first xs))
]

So what @racket[write-byte] is doing is printing something
and producing void. If we were to sequence
@racket[write-byte] using @racket[begin], would print
something and produce a non-void value:

@ex[
(begin (write-byte 97)
       #t)
]

Notice how the REPL in the notes is helpfully using color to
distinguish the printed output from the program and the
result of the program.

Now's let's look at @racket[read-byte]. It takes no
arguments and reads a byte from stdin. If there's no more
bytes on stdin, it produces @racket[eof]. Its cousin
@racket[peek-byte] also gets a byte from stdin, but it
leaves the stream intact so that the same byte would be read
the next time around.

Now, making examples of @racket[read-byte] is a bit more
tricky. While @racket[write-byte] interacts with the outside
world by printing something to stdout, what it prints is
determined by the program text. On the other hand, what
@racket[read-byte] reads depends on what's in the stdin
stream. If you launch Racket and type @racket[(read-byte)],
Racket will then block, waiting for you to type something,
so that a byte can be read. It will produce whatever the
first byte of what you type is.

So how can I make examples?

One option is to use the operating system shell to ``pipe''
output from one program as input to the Racket process,
which will then read that data as it's input. Together with
@tt{printf} program, we can write the data we want the
Racket program to see on stdin:

@shellbox["printf 'hello' | racket -e '(read-byte)'"]
@shellbox["printf 'hello' | racket -e '(list (read-byte) (read-byte))'"]

If we pipe the empty string, the program will produce the
@racket[eof] value:

@shellbox["printf '' | racket -e '(read-byte)'"]

Another possibility is to use a similar mechanism but from
within Racket. The @racket[with-input-from-string] uses a
given string as the data available on stdin. Then
@racket[(read-byte)] will read from this data:

@ex[
 (with-input-from-string "hello" (λ () (read-byte)))
 (with-input-from-string "hello" (λ () (list (read-byte) (read-byte))))
 (with-input-from-string "" (λ () (read-byte)))
]

This uses @racket[with-input-from-string] which takes a
string and a zero-argument function. It then installs the
contents of the string in stdin as though you had typed this
data, then invokes the function, thereby running a
computation with a predetermined input stream.

There's a matching @racket[with-output-to-string] function
that takes a zero-argument function and runs it in a way
that collects the output and produces it as a string. This
let's you capture what was printed and turn it in to a value
that is produced:

@ex[
(with-output-to-string (λ () (write-byte 104)))
]

These facilities will be useful for making examples, but also
for writing test cases, since we can set up the state of
the outside world and capture changes as values, which we
can then use to assert the expected behavior:

@ex[
 (check-equal?
  (with-input-from-string "hello"
    (λ ()
      (with-output-to-string
	(λ ()
	  (write-byte (read-byte))))))
  "h")]


@section{Meaning of Evildoer programs}

Here's an interpreter for Evildoer:

@codeblock-include["evildoer/interpreter/interp.rkt"]

The interpretation of primitives relies on the
underlying implementations @racket[read-byte],
@racket[write-byte], etc. from Racket (just like it does
for all the other operations):

@codeblock-include["evildoer/interpreter/interp-prim.rkt"]

Interpreting a program that reads and writes will itself
read and write:

@ex[
 (interp (parse '(write-byte 104)))
 (with-input-from-string "hello"
   (λ ()
     (interp (parse '(write-byte (read-byte))))))
 ]

Using @racket[with-input-from-string] and
@racket[with-output-to-string], we can also build a useful utility for
interpreting programs with strings representing stdin and stdout:

@codeblock-include["evildoer/interpreter/interp-io.rkt"]

@ex[
 (interp/io (parse '(write-byte 104)) "")
 (interp/io (parse '(write-byte (read-byte))) "hello")
 ]

This is useful to write tests about programs that have
side-effects because we can turn what was an effectful
computation into a pure one:

@ex[
 (check-equal? (interp/io (parse '(write-byte (read-byte))) "hello")
	       (cons (void) "h"))
 ]

@;{
OK, so now, what about the formal mathematical model of
Evildoer? We have to reconsider the domain of program
meanings. No longer does an expression just mean a value;
its meaning may depend upon the state of the input and
output port.  Moreover, an expression may @emph{alter} the
state of these ports.

There are several different approaches we might take to
formally model the effects of @racket[read-byte] and
@racket[write-byte]. We'll adopt a fairly simple one which
is to say that the semantic function now takes and produces
a pair of ports, which we can model as a list of bytes.
Reading from the input port consumes elements from the input
bytes, while writing to the output port appends elements.
This is pretty close in spirit to our @racket[interp/io]
facility, but instead of capturing the effects with string ports,
we will define the meaning of effects directly.

(Semantics omitted for now.)
}

@section{Encoding values in Evildoer}

With new values, namely the void and eof values, comes the need to add
new bit encodings. So we add new encodings for @racket[eof] and
@racket[void], for which we simply pick two unused bit patterns:
@binary[(value->bits eof)] and @binary[(value->bits (void))],
respectively.

@codeblock-include["evildoer/runtime/types.rkt"]

@section[#:tag "calling-c"]{Detour: Calling external functions}

Some aspects of the Evildoer compiler will be straightforward, e.g.,
adding @racket[eof], @racket[(void)], @racket[eof-object?], etc.
There's conceptually nothing new going on there.  But what about
@racket[read-byte], @racket[write-byte] and @racket[peek-byte]?  These
will require a new set of tricks to implement.

We have a couple of options for how to approach these primitives:

@itemlist[#:style 'ordered

@item{generate assembly code for issuing operating system calls to
do I/O operations, or}

@item{assume some code in the stand-alone runtime system or the host
system will provide the functionality to I/O and have the compiler
emit calls to those external functions.}

]

The first option will require looking up details for system calls on
the particular operating system in use, generating code to make those
calls, and adding logic to check for errors.  For the second option,
we can leave that up to the runtime or host system and produce code
that calls this code.  This is the simpler approach and the one we
adopt.

In order to call code potentially written in a different language and
compiled by a different compiler, we have to conform to the System V
ABI calling convention, which is essentially an agreement on how calls
should work (how are arguments passed, how are return values returned,
etc.) that enables calls to work at the level of assembly code.

Up to this point, we've seen how C code can call code written in
assembly as though it were a C function.  To go the other direction,
we need to explore how to make calls to functions written in C (or
whatever else) from assembly.  Let's look at that now.

@margin-note{If you haven't already, be sure to read up on how calls
work in @secref{a86}.}

Let's start of by assuming our runtime system or host is going to
provide a @tt{gcd} function for computing the greatest common divisor
of two integers given as arguments to the function.

Now, how can we call @tt{gcd} from aseembly code?  Just as there is a
convention that a return value is communicated through @racket[rax],
there are conventions governing the communication of arguments. The
conventions are known as an @bold{Application Binary Interface} or
ABI.  The set of conventions we're following is called the
@bold{System V} ABI, and it used by Unix variants like Mac OS, Linux,
and BSD systems. (Windows follows a different ABI.)

The convention for arguments is that the first six integer
or pointer parameters are passed in the registers
@racket[rdi], @racket[rsi], @racket[rdx], @racket[rcx],
@racket[r8], @racket[r9]. Additional arguments and large
arguments such as @tt{struct}s are passed on the stack.

So we will pass the two arguments of @tt{gcd} in registers
@racket[rdi] and @racket[rsi], respectively, then we use the
@racket[Call] instruction to call @tt{gcd}.  Suppose we want to
compute @tt{gcd(36,60)}:

@#reader scribble/comment-reader
(ex
(define example
  (prog (Global 'entry)
	(Label 'entry)
	(Mov 'rdi 36) ; first arg
	(Mov 'rsi 60) ; second arg
	(Sub 'rsp 8)  ; align stack to 16-bytes
	(Extern 'gcd)
	(Call 'gcd)   ; call extern gcd
	(Sal 'rax int-shift) ; convert result to a value
	(Add 'rsp 8)  ; restore stack
	(Ret))))

A few things to notice in the above code:

@itemlist[#:style 'ordered

@item{The label @racket['gcd] is declared to be external with
@racket[Extern], this means the label is used but not defined in this
program.  We placed this declaration immediately before the @racket[Call]
instruction, but it can appear anywhere in the program.}

@item{The stack pointer register @racket[rsp] is decremented by
@racket[8] before the @racket[Call] instruction and then incremented
by @racket[8] after the call returns.  This is to ensure the stack is
aligned to 16-bytes for call; a requirement of the System V ABI.}

@item{For consistency with our run-time system, we return the result
encoded as an integer value, which is accomplished by shifting the
result in @racket[rax] to the left by @racket[#,int-shift].}

]

We could attempt to run this program with @racket[asm-interp], but
it will complain about @tt{gcd} being an undefined label.  That makes
sense since we haven't actually said what we want @racket['gcd] to
mean.

@ex[
(eval:error (bits->value (asm-interp example)))]


The idea is that either our standalone runtime could provide the
@racket['gcd] function, perhaps written in C, or the host language
Racket could provide the functionality.  Let's look at both
approaches.

@subsection{Calling host functions from @racket[asm-interp]}

From within Racket it is possible to provide function definitions for
external labels when running code with @racket[asm-interp].  The way
this works is we parameterize the assembly interpeter's current set of
external labels and provide:

@itemlist[

@item{the name of the label we want to link,}

@item{a Racket function that will be called when the label is called,}

@item{a function signatures for the Racket function that describes how
the arguments and return values are encoded and decode as Racket values.}]

@#reader scribble/comment-reader
(ex
(require ffi/unsafe) ; for _fun, _int64, etc.
(parameterize
  ([current-externs
    (list (extern 'gcd gcd (_fun _int64 _int64 -> _int64)))])
  (bits->value
    (asm-interp example))))

Here we are declaring that the Racket @racket[gcd] function can be
called using the @racket['gcd] label following the System V ABI
calling convention.  The bit arguments are automatically converted to
Racket integers when called, and the Racket integer result is convert
to bits when @racket[gcd] returns.

@subsection{Calling functions in the stand-alone runtime system}

Host provided functions are useful for testing and exploration, but
for a stand-alone executable, we want the runtime system to provide
that functionality.  The compiler doesn't change, but instead we will
link the object code produced by the compiler with a definition of
@tt{gcd}.

First, let's write that function in C.

@filebox-include[fancy-c evildoer "gcd.c"]

We can compile this into an object file:

@shellbox["clang -c gcd.c"]

Now let's save our example program as an assembly file:

@ex[
 (with-output-to-file "example.s"
   (λ ()
     (asm-display example))
   #:exists 'truncate)]

Now we can assemble it into an object file, link the objects together
to make an executable, and then run it:

@shellbox["clang -c example.s"
	  "clang -c gcd.c"
	  "make -C runtime"
	  "clang example.o gcd.o runtime/runtime.o -o example"
	  "./example"]

So now we've seen the essence of how to call functions from assembly
code, which opens up an implementation strategy for implementing
features: write C code as part of the run-time system and call it from
the compiled code.


@section{Hosted I/O primitives}

We've now seen how functionality can be made available to compiled
programs, either as Racket hosted functions with @racket[asm-interp] or
as functions linked into the stand-alone run-time system.

Our approach to adding @racket[read-byte], @racket[write-byte] and
@racket[peek-byte] is going to be to assume we have functions
@tt{read_byte}, @tt{write_byte}, and @tt{peek_byte} available, either
from the host or runtime, and have the compiler generate calls to
these functions according to the System V ABI calling convention.
Both @tt{read_byte} and @tt{peek_byte} will take no arguments and
return either a byte value or the eof @emph{value}, while
@tt{write_byte} takes a byte value and returns the eof value.

Let us first use the hosted approach, then develop the compiler, then
we can return to issue of the stand-alone runtime system.

To host these primitives, we will make the Racket @racket[read-byte],
@racket[write-byte], and @racket[peek-byte] callable from assembly
code.  The only problem is that these function produce and consume
Racket values, not Evildoer values encoded as bits.  We can solve this
issue by wrapping the functions to decode and encode on the way in and
out.  For example:

@ex[
(define (prim-read-byte)
  (value->bits (read-byte)))

(define (prim-peek-byte)
  (value->bits (peek-byte)))

(define (prim-write-byte bs)
  (value->bits (write-byte (bits->value bs))))
]

Now it's important to observe that these functions always return bit
encodings of values, which is exactly what our caller code will
expect.

@ex[
(with-input-from-string "hello"
  (lambda ()
    (prim-read-byte)))

(with-input-from-string ""
  (lambda ()
    (prim-read-byte)))
]

And if we want to call @racket[prim-write-byte], we need to provide a
bit encoding of a byte.

@ex[
(with-output-to-string
  (lambda ()
    (prim-write-byte (value->bits 97))))
]

But now we are in a good position to enrich @racket[asm-interp] with
this functionality.  Let's make this part of the language
implementation.  We'll define a @racket[asm-interp/host] function that
includes all of the functionality we expect for the Evildoer language.

@codeblock-include["evildoer/executor/host.rkt"]

Now let's write an assembly program that calls some of these
functions.

@ex[
(define read-write-byte
  (prog
    (Global 'entry)
    (Extern 'read_byte)
    (Extern 'write_byte)
    (Label 'entry)
    (Sub rsp 8)
    (Call 'read_byte)
    (Mov rdi rax)
    (Call 'write_byte)
    (Add rsp 8)
    (Ret)))]

This program reads a byte with @tt{read_byte} and then immediately
writes that byte with @tt{write_byte} and returns, so the return value
should be void since that's what will be in the @racket[rax] register
at the end.

@ex[
(with-input-from-string "hello"
  (lambda ()
    (asm-interp/host read-write-byte)))]

Notice that this prints "h" and return @racket[#,(value->bits (void))].

@section{A Compiler for Evildoer}


Implementing @racket[eof], @racket[void],
@racket[eof-object?] and @racket[begin] are all
straightfoward and don't really involve anything new.

For @racket[peek-byte], @racket[read-byte], and
@racket[write-byte], we generate code that calls the
appropriate external function. In the case of @racket[write-byte],
we arrange for the byte that we'd like to write to be in
@racket[rdi] before the call.

Finally, since the emitted code is potentially issuing calls
to external functions, we make sure to align the stack to
16-bytes. Rather than do this at each call site, we take
advantage of the fact that no other stack changes occur and
adjust the stack just once at the entry and exit of the
code.

The top-level compiler:

@codeblock-include["evildoer/compiler/compile.rkt"]

The primitive operation compiler:

@codeblock-include["evildoer/compiler/compile-ops.rkt"]


Notice how expressions like @racket[(read-byte)] and @racket[(write-byte)]
compile to calls into the run-time/host system:

@ex[
(compile-op0 'read-byte)
(compile-op1 'write-byte)]

So we can write some nice high-level examples:

@ex[
(asm-interp/host (compile (parse '(write-byte 97))))]


@section{Testing and correctness}


Let's define an @racket[exec] function that should be equivalent to
@racket[interp].  We can also define an @racket[exec/io] function
equivalent to @racket[interp/io] that's useful for testing programs
that do I/O.

@codeblock-include["evildoer/executor/exec.rkt"]

And we can confirm that it works as expect:

@ex[
(exec (parse '(write-byte 97)))
(exec/io (parse '(write-byte 97)) "")
(exec/io (parse '(write-byte (read-byte))) "a")
(exec/io (parse '(eof-object? (read-byte))) "")]


We can now state the correctness property we want of the compiler:

@bold{Compiler Correctness}: @emph{For all @racket[e] @math{∈}
@tt{Expr}, @racket[i], @racket[o] @math{∈} @tt{String}, and @racket[v]
@math{∈} @tt{Value}, if @racket[(interp/io e i)] equals @racket[(cons
v o)], then @racket[(exec/io e i)] equals
@racket[(cons v o)].}

Testing compiler correctness is updated as follows, notice it now
takes an additional parameter representing the state of the input
stream:

@codeblock-include["evildoer/correct.rkt"]

@racketblock[
(check-compiler (parse '(void)) "")
(check-compiler (parse '(read-byte)) "a")
(check-compiler (parse '(write-byte 97)) "")]

The @racket[random-expr] function generates random expressions and
@racket[random-well-defined-expr] generates random expressions that are
guaranteed to be well-defined, as usual.  Additionally, the
@racket[random-input] function produces a random string that can be
used as the input.

@racketblock[
(require "syntax/random.rkt")
(random-expr)
(random-well-defined-expr)
(random-input)]

Together, these can be used to randomly test the correctness of the
compiler:

@racketblock[
(for ((i 100))
  (check-compiler (random-expr) (random-input)))
(for ((i 100))
  (check-compiler (random-well-defined-expr) (random-input)))]


@section{Stand-alone runtime system for Evildoer}

With new values comes the need to add new bit encodings. So
we add new encodings for @racket[eof] and @racket[void]:

@filebox-include[fancy-c evildoer "runtime/types.h"]

The interface for the run-time system is extended to include
file pointers for the input and output ports:

@filebox-include[fancy-c evildoer "runtime/runtime.h"]

The main entry point for the run-time sets up the input and output
pointers to point to @tt{stdin} and @tt{stdout} and is updated
to handle the proper printing of a void result:

@filebox-include[fancy-c evildoer "runtime/main.c"]

But the real novelty of the Evildoer run-time is that there
will be new functions that implement @racket[read-byte],
@racket[peek-byte], and @racket[write-byte]; these will be C
functions called @racket[read_byte], @racket[peek_byte] and
@racket[write_byte]:

@filebox-include[fancy-c evildoer "runtime/io.c"]

This functionality is implemented in terms of standard C library
functions @tt{getc}, @tt{ungetc}, @tt{putc} and the run-time system's
functions for encoding and decoding values such as
@tt{val_unwrap_int}, @tt{val_wrap_void}, etc.
