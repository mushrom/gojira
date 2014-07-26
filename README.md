Gojira
======
A scheme interpreter

A work in progress, and is currently missing some major pieces of the language.

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
Currently does not support proper syntax definitions, (explicit) continuations, optimized tail calls, and a lot of the standard library.
But it does print "Hello world".
