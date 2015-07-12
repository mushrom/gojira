#!/usr/bin/env gojira

(import! 'lists)

(define make-hello
  (lambda (str)
    (define :mut localstr str)
    (lambda ()
      (define localstr (string-append "hello, " localstr))
      localstr)))

(define world (make-hello "world!"))

(foreach (iota 5)
  (lambda (n) (print (world))))
