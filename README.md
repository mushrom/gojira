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

    ./out/gojira example/hello.scm

    # To run multiple files in the same environment:
    ./out/gojira libs/base.scm libs/math.scm example/misc.scm

    # To continue interpreting after finished running:
    ./out/gojira -i example/hello.scm

Status
- - - -

Currently supports:

- Basic scheme interpreting with some standard operators (+, -, display, null?, etc)
- Tail call elimination
- garbage collection
- (incomplete) macros/syntax extensions

Coming soon:

- First-class continuations
- External (C function) linking
