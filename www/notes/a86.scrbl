#lang scribble/manual

@(require (for-label (except-in racket compile)
                     a86))

@(require scribble/examples
	  redex/reduction-semantics
          redex/pict
	  (only-in pict scale)
	  (only-in racket system)
	  "../fancyverb.rkt"
	  "../utils.rkt"
	  "utils.rkt"
	  "ev.rkt")

@(ev '(require rackunit a86))

@(ev `(current-directory ,(path->string a86)))

@(define (shellbox . s)
   (parameterize ([current-directory a86])
     (filebox (emph "shell")
              (fancyverbatim "fish" (apply shell s)))))

@; compile time generation of tri.s and main.c so they can be listed
@(require (for-syntax racket/base "utils.rkt"))
@(begin-for-syntax
   (require racket/system a86/ast a86/printer)
   (define (tri n)
      (list (Global 'entry)
         (Label 'entry)
         (Mov 'rbx n)      (% "the \"input\"")
         (%%% "tri: a recursive function for computing nth")
         (%%% "triangular number, where n is given in rbx.")
         (Label 'tri)
         (Cmp 'rbx 0)      (% "if rbx = 0, done")
         (Je 'done)
         (Push 'rbx)       (% "save rbx")
         (Sub 'rbx 1)
         (Call 'tri)       (% "compute tri(rbx-1) in rax")
         (Pop 'rbx)        (% "restore rbx")
         (Add 'rax 'rbx)   (% "result is rbx+tri(rbx-1)")
         (Ret)
         (Label 'done)     (% "jump here for base case")
         (Mov 'rax 0)      (% "return 0")
         (Ret)))

   (define main.c
     #<<HERE
#include <stdio.h>
#include <inttypes.h>

int64_t entry();

int main(int argc, char** argv) {
  int64_t result = entry();
  printf("%" PRId64 "\n", result);
  return 0;
}
HERE
     )

   (define gcd.c
     #<<HERE
int gcd(int n1, int n2) {
    return (n2 == 0) ? n1 : gcd(n2, n1 % n2);
}
HERE
     )

   (parameterize ([current-directory a86])
     (save-file "tri.s" (asm-string (tri 36)))
     (save-file "main.c" main.c)
     (save-file "gcd.c" gcd.c)))


@title[#:tag "a86"]{a86: a Little Assembly Language}

@emph{You need to let the little things that would ordinarily bore you suddenly thrill you.}

@table-of-contents[]

@section[#:tag "a86-Overview"]{Overview}

x86 is an instruction set architecture (ISA), which is a
fancy way of saying a programming language whose interpreter
is implemented in hardware. Really, x86 is a family of
instruction set architectures, and the first member of the
family was 8086, a 16-bit ISA for Intel's 8086 chipset.

x86 is old. It's older than the professors teaching this
class. But it lives on today in Intel and AMD based
computers in the form x86-64, the 64-bit descendant of the
8086 language.

Because it's old and because the design of each generation
of x86 has had significant backwards compatability
requirements and because modern processors are sophisticated
pieces of machinery, the x86-64 language is, well,
complicated. For example,
@link["https://software.intel.com/content/dam/develop/external/us/en/documents-tps/325462-sdm-vol-1-2abcd-3abcd.pdf"]{
 Intel's x86 software developer's manual} is 5,066 pages
long.
@link["https://www.amd.com/content/dam/amd/en/documents/processor-tech-docs/programmer-references/40332.pdf"]{
 AMD's manual} is 3,242 pages.


x86-64 is going to be used as the target language of our
compiler. We'll build up a translation from a very
high-level language, based on Racket, down to the very
low-level langauge of x86-64.

However, that doesn't mean we need to digest 8K+ pages of
dense technical manuals. We will only use very small
fraction of the x86-64 instruction set.

To make our lives easier, we will do what programming
language designers often do, we will abstract the behemoth
of x86-64 to a small, core language (which we call @bold{
 a86}). Our compilers will target a86 and compiling from a86
to x86 as the last step in the compiler pipeline will be
dead simple.

This chapter describes the a86 language.

@section{Giving x86 a try}

Before describing a86, let's take a brief look at x86.

There are a few different human-readable formats for writing
x86 assembly code, but we'll be using the one supported by
@link["https://www.nasm.us/"]{the Netwide Assembler} (NASM).

Here's an example x86 program, written using nasm syntax.
The program has one global label called @tt{entry}, which
will be the main entry point for the program. This program
computes the 36th triangular number, which will reside in
register @tt{rax} when the code returns.

@margin-note{The conventions for label names differ between Mac and
Linux systems.  On MacOS, you need to prefix all label names with an
underscore, while on Linux you do not.  So on a Mac, you would use the
names @tt{_entry}, @tt{_tri}, and @tt{_done}, while on Linux you would
use @tt{entry}, @tt{tri}, and @tt{done}.

This example is shown using the @(if (eq? (system-type 'os) 'macosx)
"MacOS" "Linux") naming convention, because that's what operating
system was used when this web page was built.}

@filebox-include[fancy-nasm a86 "tri.s"]

The @math{n}th triangular number is the sum of the integers
from 0 to @math{n}, so the @math{36}th triangular number is
@math{0 + 1 + 2 + 3 + ... + 34 + 35 + 36}.

This code is not intended to be a model of efficient
computation, rather it demonstrates some important x86
instructions and shows how to compute in a recursive style,
even at a low-level.

Without getting too bogged down in the details, here how the
code works. Instructions execute one after another. There
are a number of registers which can be used to hold values.
This code makes use of the @tt{rax} and @tt{rbx} register
(and some other registers are implicitly used and altered by
the @tt{call}, @tt{push}, @tt{pop} and @tt{ret}
instructions). The lines like @tt{entry:}, @tt{tri:}, and
@tt{done:} are not instructions, but labels -- they are
names for locations in the source code and can be used as
the targets of jumps and calls.

Suppose we start executing at @tt{entry}.

@itemlist[
 @item{@tt{mov rbx, 36} sets the @tt{rbx} register to 36.}

 @item{@tt{cmp rbx 0} compares the value in register @tt{
   rbx} to zero. Executing this instruction sets flags in the
  CPU, which affect subsequent ``conditional'' instructions.
  In this program, the next instruction is a conditional jump.}

 @item{@tt{je done} either jumps to the instruction
  following label @tt{done} or proceeds to the next
  instruction, based on the state of the comparison flags. The
  @tt{je} instruction jumps if the comparison was equal, so
  control jumps to done if @tt{rbx} was 0 in this program. If
  not, the next instruction is executed.}

 @item{@tt{push rbx} uses memory as a stack to save the
  value of @tt{rbx}. Under the hood this is modifying a
  register that holds a pointer to the stack memory location
  (register @tt{rsp}).}

 @item{@tt{sub rbx, 1} decrements @tt{rbx} by 1.}

 @item{@tt{call tri} performs something like a function
  call; it uses memory as a stack to save the current location
  in the code (which is where control should return to after
  the function has completed). After saving this return
  pointer, it jumps to the label @tt{tri}. There aren't really
  functions, but this uses the stack to mimic the
  call-and-return mechanism of functions.}

 @item{@tt{pop rbx} uses the stack memory to pop off the top
  element and move it into @tt{rbx}, adjusting the stack
  pointer appropriately.  This has the effect of restoring @tt{rbx}
  to the value saved earlier by the @tt{push}, i.e. before the decrement
  and any changes done in the call to @tt{tri}.}

 @item{@tt{add rax, rbx} updates @tt{rax} to hold @tt{rax}
  plus @tt{rbx}.}

 @item{@tt{ret} does a ``return,'' i.e. it pops an address
  from the stack and jumps to it. In this case, the jump
  either returns from to a previous call to @tt{tri} or to
  original caller of @tt{entry}.}

 @item{@tt{mov rax, 0} this instruction is only reached from
  the earlier conditional jump. It sets @tt{rax} to 0. This
  program computes its result in @tt{rax} so this is saying
  that when @tt{rbx} (the ``input'') is 0, then (the
  ``output'') is 0.}

 @item{@tt{ret} does a ``return,'' either to a prior call to
  @tt{tri} or the caller of @tt{entry}.}
 ]

Despite the lower-level mechanisms, this code computes in a
way similar to a non-tail recursive definition of the
@racket[tri] function written in Racket:

@racketblock[
 (define (tri n)
   (if (= n 0)
       0
       (+ n (tri (sub1 n)))))
 (tri 36)
]

@margin-note{As an exercise to check your understanding, try writing a
tail-recursive version of @racket[tri] and the corresponding
x86 code, which should not need to push anything to the
stack or use the @tt{call} instruction.}

We can compile the @tt{tri.s} assembly program to an object
file with @tt{nasm}:

@margin-note{The format argument should be @tt{macho64} on
 Mac OS and @tt{elf64} on Linux.}

@shellbox[
 (format "nasm -f ~a -o tri.o tri.s"
         (if (eq? 'macosx (system-type 'os))
             "macho64"
             "elf64"))]

To run the object file, we will need to link with a small C program
that can call the @tt{entry} label of our assembly code and then
print the result:

@filebox-include[fancy-c a86 "main.c"]

Notice that from the C program's perspective, the assembly
code defines what looks like a C function called @tt{entry}
that returns an @tt{int64_t} result.

How does this work? When the C program calls @tt{entry} it
places a return pointer on the stack and jumps to @tt{
 entry}. The fact that we decided to put the result in
register @tt{rax} was not arbitrary -- that's the register
that by convention is used for a return value. When the
assembly code executes it's final @tt{ret} instruction, it
jumps back to C with the 36th triangular number in @tt{rax},
which the C side knows is the return value. This convention
is part of a larger set of conventions known as the @bold{
 Application Binary Interface}. For a reference, see the
@secref{Texts} section of the notes.


We can compile the @tt{main.c} C program to an object file with @tt{gcc}:

@shellbox[
 "gcc -c main.c -o main.o"
 ]

Now we can make an executable by linking the two together:

@shellbox[
 "gcc main.o tri.o -o tri"
]

Finally, we can run the executable:

@shellbox[
 "./tri"
]

There, of course, is a lot more to x86-64 than what's been
shown here. If you want to dig deeper, check the references
in @secref{Texts}.  But now let's turn to a86.

@section{a86: Representing x86 Code as Data}

Here we will employ one of the great ideas in computer
science: we will represent programs as data. Rather than
toil away at the level of x86, writing programs directly in
nasm syntax, compiling, and running them, we will instead
design a data type definition for representing x86 programs
and @emph{compute} programs.

Our representation will not be complete -- this is going to
help us simplify x86 down to something manageable. We won't
cover every instruction and we won't cover every variation
of the instructions we do cover.

An a86 program is a list of a86 instructions. Each
instruction is represented as a structure, described in the
following section.

@margin-note{Before working through these examples, you'll need to
install the @racketmodname[a86] module, part of the @tt{langs} package
for this course.  See @secref{langs-package} for details on
installing.}

Here's the triangular number example:

@margin-note{@racket[%], @racket[%%], and @racket[%%%] are
 constructors for assembly comments.}

@#reader scribble/comment-reader
 (ex
 ; import the a86 library
 (require a86)
 ; a86 code that computes the 36th triangular number
 (define tri-36
   (list (Global 'entry)
         (Label 'entry)
         (Mov 'rbx 36)     (% "the \"input\"")
         (%%% "tri: a recursive function for computing nth")
         (%%% "triangular number, where n is given in rbx.")
         (Label 'tri)
         (Cmp 'rbx 0)      (% "if rbx = 0, done")
         (Je 'done)
         (Push 'rbx)       (% "save rbx")
         (Sub 'rbx 1)
         (Call 'tri)       (% "compute tri(rbx-1) in rax")
         (Pop 'rbx)        (% "restore rbx")
         (Add 'rax 'rbx)   (% "result is rbx+tri(rbx-1)")
         (Ret)
         (Label 'done)     (% "jump here for base case")
         (Mov 'rax 0)      (% "return 0")
         (Ret)))
)

This code should look familiar. At first glance it's just
the x86 code with more parentheses. @margin-note{And who
 doesn't love more parentheses?} But something fundamental
has happended. This a86 program is just a value in Racket.
This means we can use Racket as a @bold{Meta-Language} to
write programs that compute @emph{with} x86 programs.

So for example, let's say you have two a86 programs and you
want to glue them together into one: well that just
@racket[append]. Suppose you want to compute which registers
are used in a given a86 program? Suppose you want to replace
uses of @racket['rax] with @racket['rdi]? It just a matter
of writing the right Racket function to do it.

Here's another immediate benefit. Instead of writing a
single x86 program, let's write an infinite set of a86
programs, one for computing each @math{n}th triangular
number.  Easy-peasy:

@#reader scribble/comment-reader
 (ex
 ; Natural -> a86
 ; Computes a86 code that computes the @math{n}th triangular number
 (define (tri n)
   (list (Global 'entry)
         (Label 'entry)
         (Mov 'rbx n)      (% "the \"input\"")
         (%%% "tri: a recursive function for computing nth")
         (%%% "triangular number, where n is given in rbx.")
         (Label 'tri)
         (Cmp 'rbx 0)      (% "if rbx = 0, done")
         (Je 'done)
         (Push 'rbx)       (% "save rbx")
         (Sub 'rbx 1)
         (Call 'tri)       (% "compute tri(rbx-1) in rax")
         (Pop 'rbx)        (% "restore rbx")
         (Add 'rax 'rbx)   (% "result is rbx+tri(rbx-1)")
         (Ret)
         (Label 'done)
         (Mov 'rax 0)
         (Ret)))

  ; recreate original program
  (define tri-36 (tri 36))
)

It's also easy to go from our data representation to its
interpretation as an x86 program.

There is a function provided for printing an a86 program as an x86
program using nasm notation, called @racket[asm-display].  Calling
this function prints to the current output port, but it's also
possible to write the output to a file or convert it to a string.

@margin-note{The @racket[asm-display] function knows what OS you are
using and adjusts the label naming convention to use underscores or
not, so that you don't have to worry about it.}

@ex[
(asm-display (tri 36))
    ]

Notice how this generates exactly what you saw in @tt{tri.s}.

From here, we can assemble, link, and execute.

We can also, since we have a general purpose programming
language at our disposal in the meta-language, write a
program to do all that for us:

@ex[
 (asm-interp (tri 36))
 ]

The @racket[asm-interp] function consumes an @tt{a86}
program as input and produces the integer result the program
computes, i.e. it is an @bold{Interpreter} for a86. Behind
the scenes it does this by converting to nasm, assemblying,
compiling a thin C wrapper, executing the program, and
reading the result. This will be a rather handy tool both in
interactively exploring the a86 language (you can write
assembly in a REPL), but also an important tool when it
comes time to test the compilers we write.


@include-section[(lib "a86/scribblings/a86.scrbl")]
