Gojira
======
A scheme interpreter

A work in progress. Intended to be a minimal but entirely usable interpreter, for fun and for my pet OS project.

To build
- - - - -
After cloning the repository,

    make
    ./out/gojira

To run a file,

    ./out/gojira -f example/misc.scm

    # To continue interpreting after finished running:
    ./out/gojira -f example/misc.scm -i

Status
- - - -

Currently supports:

- Basic scheme interpreting with some standard operators (+, -, display, null?, etc)
- Tail call elimination
- (incomplete) macros/syntax extensions
- (incomplete) garbage collection

Coming soon:

- First-class continuations
- External (C function) linking
