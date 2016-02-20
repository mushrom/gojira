Gojira
======
A nifty lisp interpreter

Gojira is a dialect of lisp derived primarily from scheme. It's intended to be
a somewhat minimal but entirely usable language, with some experimental ideas
and practicality as a goal.

It is something of a pet project for me; a bit more serious than a toy project,
but I'm not going to advise you use it over python, etc (yet).

It's also fairly portable, relying on a small subset of the libc for core functionality.

To build
- - - - -

    git clone --recursive https://github.com/dragontux/gojira.git
    cd gojira
    ./configure   # the default prefix is /usr/local, use --prefix= to change it
    make
    make install
    make test

To run a file,

    gojira example/colors.scm

    # To run multiple files in the same environment:
    gojira example/colors.scm example/calc.scm

    # To continue interpreting after finished running:
    gojira -i example/fizzbuzz.scm

Status
- - - -

Currently supports:

- Decent scheme support with some standard operators (+, -, map, iota, etc)
- Mark-and-sweep garbage collection
- UTF-8 variable names
- (Incomplete) syntax-rules
- Sort of python-ish iterators
- Hashmaps
- Applicable booleans
- Networking with TCP
- A simple module system
- Modules for documention, ansi terminal colors, input prompts, ...
- A (also incomplete) C API for using gojira as an extension language
- some r6rs bytevector functions
- And more!
- Order now and recieve up to 20% more source

Coming soon:

- Tail call elimination
- First-class continuations
- External (C function) linking
