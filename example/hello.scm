#!/usr/bin/env gojira

(import! 'lists)

(define make-hello
  (lambda (str)
    (lambda ()
      (define str (string-append "hello, " str))
      str)))

(define world (make-hello "world!"))

(foreach (iota 5)
  (lambda (n) (print (world))))
