# nokolisp
Lisp interpreter and compiler from 1977-1988 for MSDOS.

First you compile nokolisp.asm to nokolisp.exe. Then you do (in (open 'boot.lsp)), then you start (\*main-loop\*), which
errors and recompiles and then you (make-exe). And then you have noko.exe.

There was some elegant way to do all this shit, but I cant remember what it was.

TRS5-files add floating point arithmetics, via Turbo Pascal interrupt vector. 

http://timonoko.github.io/Nokolisp.htm
