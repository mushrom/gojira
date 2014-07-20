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
Currently does not support syntax definitions, closures (lazy binding of variables), continuations,
tail call recursion, a lot of the standard library, or even working garbage collection.
But it does print "Hello world".
