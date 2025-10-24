#lang scribble/manual

@(require (for-label (except-in racket ... compile) a86/printer a86/ast))
@(require redex/pict
          racket/runtime-path
          scribble/examples
	  evildoer/types
          "../fancyverb.rkt"
	  "utils.rkt"
	  "ev.rkt"	  
	  "../utils.rkt")

@(define (shellbox . s)
   (parameterize ([current-directory (build-path langs "evildoer")])
     (filebox (emph "shell")
              (fancyverbatim "fish" (apply shell s)))))


@(define codeblock-include (make-codeblock-include #'h))

@(ev '(require rackunit a86 evildoer evildoer/correct evildoer/compile-ops))
@;{This is needed for the example that uses current-objs}
@(ev `(current-directory ,(path->string (build-path langs "evildoer"))))

@(require (for-syntax racket/base))
@(begin-for-syntax
   (require "utils.rkt")
   (parameterize ([current-directory (build-path langs "evildoer")])
     (save-file "simple.c"
#<<HERE
#include <stdio.h>
#include <inttypes.h>

int64_t entry();

int main(int argc, char** argv) {
  printf("%" PRId64 "\n", entry());
  return 0;
}
HERE
)
     (save-file "life.c"
#<<HERE
#include <inttypes.h>

int64_t meaning(void) {
  return 42;
}
HERE
)
     (save-file "double.c"
#<<HERE
#include <inttypes.h>

int64_t dbl(int64_t x) {
  return x + x;
}
HERE
)))


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

@codeblock-include["evildoer/ast.rkt"]

The s-expression parser is defined as follows:

@codeblock-include["evildoer/parse.rkt"]

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

@codeblock-include["evildoer/interp.rkt"]

The interpretation of primitives relies on the
underlying implementations @racket[read-byte],
@racket[write-byte], etc. from Racket (just like it does
for all the other operations):

@codeblock-include["evildoer/interp-prim.rkt"]

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

@codeblock-include["evildoer/interp-io.rkt"]

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

@codeblock-include["evildoer/types.rkt"]

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

@item{add C code for I/O primitives in the run-time and generate
assembly code for calling them.}

]

The first option will require looking up details for system calls on
the particular operating system in use, generating code to make those
calls, and adding logic to check for errors.  For the second option,
we can simply write C code that calls standard functions like
@tt{getc}, @tt{putc}, etc. and let the C compiler do the heavy lifting
of generating robust assembly code for calling into the operating
system.  The compiler would then only need to generate code to call
those functions defined in the run-time system.  This is the simpler
approach and the one we adopt.

Up to this point, we've seen how C code can call code written in
assembly as though it were a C function.  To go the other direction,
we need to explore how to make calls to functions written in C from
assembly.  Let's look at that now.

@margin-note{If you haven't already, be sure to read up on how calls
work in @secref{a86}.}



Once you brushed up on how calls work, you'll know you can
define labels that behave like functions and call them.


Instead of @racket[read-byte] and friends, let's first start with
something simpler.  Imagine we want a function to compute the greatest
common divisor of two numbers.  We could of course write such a
function in assembly, but it's convenient to be able to write it
in a higher-level language like C:

@filebox-include[fancy-c evildoer "gcd.c"]

We can compile this into an object file:

@(define format (if (eq? (system-type 'os) 'macosx) "macho64" "elf64"))
          
@shellbox["gcc -c gcd.c -o gcd.o"]

Now, how can we call @tt{gcd} from aseembly code?  Just as there is a
convention that a return value is communicated through @racket[rax],
there are conventions governing the communication of arguments. The
conventions are known as an @bold{Application Binary Interface} or
ABI.  The set of conventions we're following is called the
@bold{System V} ABI, and it used by Unix variants like Mac OS, Linux,
and BSD systems. (Windows follows a different ABI.)

The convention for arguments is that the first six integer
or pointer parameters are passed in the registers
@racket['rdi], @racket['rsi], @racket['rdx], @racket['rcx],
@racket['r8], @racket['r9]. Additional arguments and large
arguments such as @tt{struct}s are passed on the stack.

So we will pass the two arguments of @tt{gcd} in registers
@racket[rdi] and @racket[rsi], respectively, then we use the
@racket[Call] instruction to call @tt{gcd}.  Suppose we want to
compute @tt{gcd(36,60)}:

@ex[
(define p
  (prog (Global 'entry)
	(Label 'entry)
	(Mov 'rdi 36)
	(Mov 'rsi 60)
	(Sub 'rsp 8)
	(Extern 'gcd)
	(Call 'gcd)
	(Sal 'rax int-shift)
	(Add 'rsp 8)
	(Ret)))]

A few things to notice in the above code:

@itemlist[#:style 'ordered

@item{The label @racket['gcd] is declared to be external with
@racket[Extern], this means the label is used but not defined in this
program.  We placed this declaration immediately before the @racket[Call]
instruction, but it can appear anywhere in the program.}

@item{The stack pointer register is decremented by @racket[8] before
the @racket[Call] instruction and then incremented by @racket[8] after
the call returns.  This is to ensure the stack is aligned to 16-bytes
for call; a requirement of the System V ABI.}

@item{For consistency with our run-time system, we return the result
encoded as an integer value, which is accomplished by shifting the
result in @racket[rax] to the left by @racket[#,int-shift].}

]
We could attempt to run this program with @racket[asm-interp], but it
will complain about @tt{gcd} being an undefined label:

@ex[
(eval:error (bits->value (asm-interp p)))]

The problem is that @racket[asm-interp] doesn't know anything about
the @tt{gcd.o} file, which defines the @tt{gcd} symbol, however,
there is a mechanism for linking in object files to the assembly
interprer:

@ex[
(current-objs '("gcd.o"))
(bits->value (asm-interp p))]

We also could create an executable using the run-time system.
To do this, first, let's save the assembly code to a file:

@ex[
 (with-output-to-file "p.s"
   (λ ()
     (asm-display p))
   #:exists 'truncate)]

Now we can assemble it into an object file, link the objects together
to make an executable, and then run it:

@shellbox[(string-append "nasm -f " format " p.s -o p.o")
          "gcc runtime.o gcd.o p.o -o p.run"
          "./p.run"]

So now we've seen the essence of how to call functions from assembly
code, which opens up an implementation strategy for implementing
features: write C code as part of the run-time system and call it from
the compiled code.

@section{A Run-Time for Evildoer}

With new values comes the need to add new bit encodings. So
we add new encodings for @racket[eof] and @racket[void]:

@filebox-include[fancy-c evildoer "types.h"]

The interface for the run-time system is extended to include
file pointers for the input and output ports:

@filebox-include[fancy-c evildoer "runtime.h"]

The main entry point for the run-time sets up the input and output
pointers to point to @tt{stdin} and @tt{stdout} and is updated
to handle the proper printing of a void result:

@filebox-include[fancy-c evildoer "main.c"]

But the real novelty of the Evildoer run-time is that there
will be new functions that implement @racket[read-byte],
@racket[peek-byte], and @racket[write-byte]; these will be C
functions called @racket[read_byte], @racket[peek_byte] and
@racket[write_byte]:

@filebox-include[fancy-c evildoer "io.c"]

This functionality is implemented in terms of standard C library
functions @tt{getc}, @tt{ungetc}, @tt{putc} and the run-time system's
functions for encoding and decoding values such as
@tt{val_unwrap_int}, @tt{val_wrap_void}, etc.

As we'll see in the next section, the main novely of the
@emph{compiler} will be that emits code to make calls to these C
functions.



@;{


Now we have all the tools needed to interact with libraries
written in C, and really any library object files that
adhere to the System V ABI. Perhaps the only remaining
wrinkle is how should we deal with the situation in which we
are using the registers that are needed to pass parameters
in a call? The answer is to save them on the stack and
restore them when the call returns. For example, suppose
@racket['rdi] held a value we wanted to use after the call
to @tt{dbl}. It's a bit contrived, but let's say we want to
use @racket['rdi] to hold the constant we'll add to the
result of calling @tt{dbl}. Now we need to save it before
writing the argument. All we need to do is add a push and
pop around the call:

The wrinkle is actually a bit deeper than this too. Suppose
we are using other registers, maybe some that are not used
for parameters, but nonetheless are registers that the
function we're calling would like to use? Without knowing
the details of how the function is implemented, we could be
defensive and save @emph{everything} we're using with the
assumption the called function may clobber anything. But
here, the ABI comes into play again. There are conventions
around who is responsible for registers in calls. The called
function is responsible for maintaining the registers
@racket['rbx], @racket['rsp], @racket['rbp], @racket['r12],
@racket['r13], @racket['r14], @racket['r15]; these are
@bold{callee-saved registers}. This means we, the callers,
don't have to worry about these registers being clobbered
and don't need to save them to the stack. If the called
function wants to use these registers, it's responsible for
saving their value and restoring them before returning. On
the other hand, registers @racket['rax], @racket['rdi],
@racket['rdx], @racket['rcx], @racket['r8], @racket['r9],
@racket['r10], and @racket['r11] are @bold{caller-saved
 registers}, which means the called function is free to
clobber them and if we want to preserve their value across
the call, we'll need to save and restore them.

As a final note, keep in mind that the compiler generates
code this is both called and a caller, so it has to be
mindful of both sides of the convention. The main entry
point @tt{entry} is called from the C run-time. If the
generated code wants to use any of the callee-saved
registers, it should save them and restore them before the
return that delivers the final result of evaluation. On the
other hand, when it calls external functions implemented in
C, it is the caller and has to maintain the caller-saved
registers.

OK, now let's use these new powers to write the compiler.

}



@section{A Compiler for Evildoer}


Implementing @racket[eof], @racket[void],
@racket[eof-object?] and @racket[begin] are all
straightfoward and don't really involve anything new.

For @racket[peek-byte], @racket[read-byte], and
@racket[write-byte], we generate code that calls the
appropriate C function. In the case of @racket[write-byte],
we arrange for the byte that we'd like to write to be in
@racket['rdi] before the call.

Finally, since the emitted code is potentially issuing calls
to external functions, we make sure to align the stack to
16-bytes. Rather than do this at each call site, we take
advantage of the fact that no other stack changes occur and
adjust the stack just once at the entry and exit of the
code.

The top-level compiler:

@codeblock-include["evildoer/compile.rkt"]

The primitive operation compiler:

@codeblock-include["evildoer/compile-ops.rkt"]


Notice how expressions like @racket[(read-byte)] and @racket[(write-byte)]
compile to calls into the run-time system:

@ex[
(compile-op0 'read-byte)
(compile-op1 'write-byte)]



@section{Testing and correctness}



We can continue to interactively try out examples with
@racket[asm-interp], although there are two issues we need
to deal with.

The first is that the @racket[asm-interp] utility doesn't know
anything about the Evildoer run-time. Hence we need to tell
@racket[asm-interp] to link it in when running an example; otherwise
labels like @tt{byte_write} will be undefined.  We saw how to do this
in @secref["calling-c"] using the @racket[current-objs] parameter to
link in object files to @racket[asm-interp].  This time, the object
file we want to link in is the Evildoer run-time.

The other is that we need to have an @racket[asm-interp/io] analog of
@racket[interp/io], i.e. we need to be able to redirect input and
output so that we can run programs in a functional way.  The
@secref["a86"] library provides this functionality by providing
@racket[asm-interp/io].  The way this function works is @emph{if}
linked objects define an @tt{in} and @tt{out} symbol, it will set
these appropriately to read input from a given string and collect
output into a string.

@ex[
(current-objs '("runtime.o"))
(asm-interp/io (compile (parse '(write-byte (read-byte)))) "a")]

Notice though, that @racket[asm-interp/io] gives back a pair
consisting of the @emph{bits} and the output string.  To match the
return type of @racket[interp/io] we need to convert the bits to a
value:

@ex[
(match (asm-interp/io (compile (parse '(write-byte (read-byte)))) "a")
  [(cons b o) (cons (bits->value b) o)])]

Using these pieces, we can write a function that matches the type signature
of @racket[interp/io]:

@codeblock-include["evildoer/exec.rkt"]

@ex[
(exec/io (parse '(write-byte (read-byte))) "z")]

Note that we still provide an @racket[exec] function, but it
assumes there is no input and it prints all output:

@ex[
(exec (parse '(eof-object? (read-byte))))
(exec (parse '(write-byte 97)))]

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

@ex[
(check-compiler (parse '(void)) "")
(check-compiler (parse '(read-byte)) "a")
(check-compiler (parse '(write-byte 97)) "")]

The @racket[random-expr] function generates random expressions and
@racket[random-well-defined-expr] generates random expressions that are
guaranteed to be well-defined, as usual.  Additionally, the
@racket[random-input] function produces a random string that can be
used as the input.

@ex[
(require "random.rkt")
(random-expr)
(random-well-defined-expr)
(random-input)]

Together, these can be used to randomly test the correctness of the
compiler:

@ex[
(for ((i 100))
  (check-compiler (random-expr) (random-input)))
(for ((i 100))
  (check-compiler (random-well-defined-expr) (random-input)))]
  