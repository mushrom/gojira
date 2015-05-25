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

    make          # Use PREFIX=/.../ to set the install location, the default is /usr/local
	make install  # PREFIX is needed for this too, if used
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

- basic scheme interpreting with some standard operators (+, -, display, null?, etc)
- garbage collection
- utf8 variable names
- (incomplete) macros/syntax extensions
- iterators
- applicable booleans

Coming soon:

- Tail call elimination
- First-class continuations
- External (C function) linking
