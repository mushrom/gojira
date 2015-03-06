; A collection of random functions to test the interpreter

(required-modules! '("srfi1" "math"))

; recursively counts down from a given number 
(define countdown)
(define countdown
  (lambda (x)
    (display "T minus ")
	(print x)
    (if (eq? x 0)
      x
      (countdown (- x 1)))))

; repeatedly perform a function for "times", using recursion
(define for
  (lambda (times f)
    (if (> times 0)
	  (begin
        (for (- times 1) f)
		(f times))
	  times)))

; repeatedly perform a function for "times", using iteration
(define for-iter
  (lambda (times f)
	(define iter
	  (lambda (count)
		(if (<= count times)
		  (begin
			 (f count)
			 (iter (seq count)))
		  count)))
	(iter 1)))

; Sort of clear the terminal
(define clear
  (lambda ()
    (for
      100
      (lambda (x) (newline)))))

; print "x" number of squares
(define psquares
  (lambda (x)
    (for-iter
      x
      (lambda (y)
        (display y)
        (display ": ")
        (print (square y))))))

; print "x" number of factorials
(define pfacts
  (lambda (x)
    (for
      x
      (lambda (y)
		(print (fact y))))))

(define wut (fact 6))

(define generator
  (lambda (x)
    (lambda ()
      (display "Called ") (display x) (display " times")
      (newline)
      (generator (+ x 1)))))

(define abc
  (lambda (count)
    (if (eq? count 0)
      0
      (begin
        (print "abc")
        (qwerty (- count 1))))))

(define qwerty
  (lambda (count)
    (if (eq? count 0)
      0
      (begin
        (print "qwerty")
        (abc (- count 1))))))

(define meh
  (lambda (y)
    (define even? 
      (lambda (x)
        (or (eq? x 0) (odd? (- x 1)))))
 
    (define-syntax odd?
      (syntax-rules ()
        ((_ x) (not (even? x)))))
 
    (even? y)))

(define beer
  (lambda (x)
    (if (> x 0)
      (begin
        (display x) (print " bottles of beer on the wall")
        (display x) (print " bottles of beer")
        (print "Take one down, pass it around")
        (display (- x 1)) (print " bottles of beer on the wall")
        (beer (- x 1)))
      #f)))

; The main function, used as the entry point
(define main
  (lambda ()
	(print "Hello, world!")

    (print "-== Factorial of 6:")
	(print wut)

    (print "-== Squares of numbers from 1 to 30:")
    (psquares 30)
    (countdown 10)))
(main)
