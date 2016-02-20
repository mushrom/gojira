#!/usr/bin/env gojira

(define (divides? num div)
  (eq? (modulo num div) 0))

(define (fizzbuzz limit)
  (for i in (iota limit 1)
    (print (cond ((divides? i 15) "FizzBuzz")
                 ((divides? i 5)  "Fizz")
                 ((divides? i 3)  "Buzz")
                  (true           i)))))

(fizzbuzz 100)
