Gojira
======
A nifty lisp interpreter

Gojira is a dialect of lisp derived primarily from scheme. It's intended to be
a somewhat minimal but entirely usable language, with some experimental ideas
and practicality as a goal.

It is something of a pet project for me; a bit more serious than a toy project,
but I'm not going to advise you use it over python, etc (yet).

To build
- - - - -
After cloning the repository,

    ./configure   # the default prefix is /usr/local, use --prefix= to change it
    make
    make install
    make test
    gojira

To run a file,

    gojira example/hello.scm

    # To run multiple files in the same environment:
    gojira example/misc.scm example/hello.scm

    # To continue interpreting after finished running:
    gojira -i example/hello.scm

Status
- - - -

Currently supports:

- Basic scheme interpreting with some standard operators (+, -, display, null?, etc)
- Garbage collection
- Utf8 variable names
- (Incomplete, unhygenic) macros/syntax extensions with pattern matching and list expansions
- Iterators
- Applicable booleans
- A simple module system

Coming soon:

- More documentation, with a built-in doc function
- Tail call elimination
- First-class continuations
- External (C function) linking
