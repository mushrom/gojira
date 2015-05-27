#!/usr/bin/env gojira
(import! 'srfi1)

(define foreach
  (lambda (xs f)
    (if (null? xs)
      '()
      (begin
        (f (car xs))
        (foreach (cdr xs) f)))))
        
(define fizzbuzz
  (lambda (limit)
    (foreach (iota limit 1)
      (lambda (x)
        (if (eq? (modulo x 15) 0) (print "FizzBuzz")
        (if (eq? (modulo x  3) 0) (print "Fizz")
        (if (eq? (modulo x  5) 0) (print "Buzz")
         else                     (print x))))))))

(fizzbuzz 100)
