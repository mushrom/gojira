; program to test the interpreter
; (proper test suite coming soon(!))
(define-syntax define
  (syntax-rules ()
    ((_ sym def)
     (intern-set 'sym def))
    ((_ sym)
     (intern-set 'sym 0))))

; this will do until proper elipsis expansion is implemented...
(define-syntax begin
  (syntax-rules ()
	((_ expr moar)
	 ((lambda () expr (begin moar))))
	((_ expr)
	 expr)
	((_) #f)))

(define-syntax while
  (syntax-rules ()
	((while condition body)
	 (begin
		(define loop
		  (lambda ()
			(if condition
			  (begin
				 body
				 (loop))
			  #f)))
		(loop)))))

(define help "To see the gojira scheme tutorial, visit http://example.com. To see the currently defined variables, try (stacktrace).")

; Recursive factorial function
(define fact
  (lambda (x)
    (if (> x 0)
      (* x (fact (- x 1)))
      1)))

; Sequence function
(define seq
  (lambda (x)
    (+ x 1)))

(define not
  (lambda (x)
    (if x
      #f
      #t)))

; TODO: Fix symbol clashes between procedures and macro expansion
(define-syntax or
  (syntax-rules ()
    ((_ _op1_ _op2_)
     (if _op1_
       #t
       (if _op2_
         #t
         #f)))))

(define-syntax and
  (syntax-rules ()
    ((_ _op1_ _op2_)
     (if _op1_
       (if _op2_
         #t
         #f)
       #f))))

(define = eq?)

(define <=
  (lambda (a b)
	(or
	  (< a b)
	  (eq? a b))))

(define caar
  (lambda (x)
	(car (car x))))

(define caaar
  (lambda (x)
	(car (caar x))))

(define print
  (lambda (x)
	(display x)
	(newline)))

; recursively counts down from a given number 
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
		  ;(begin
          ((lambda ()
			 (f count)
			 (iter (seq count))
             ))
		  count)))
	(iter 1)))

; Square a number
(define square
  (lambda (x)
    (* x x)))

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

; print every element in a list
(define asdf
  (lambda (ls)
    (define iter
      (lambda (x indent)
        (if (null? x)
          x
          (if (list? (car x))
            ((lambda ()
               (display "(")
               (iter (car x) (+ indent 4))
               (if (null? (cdr x))
                  (display ")")
                  (display ") "))
               (iter (cdr x) indent)))
            ((lambda ()
              ;(for indent (lambda (y) (display " ")))
              (display (car x))
              (if (null? (cdr x))
                 #f
                 (display " "))
              (iter (cdr x) indent)))))))

    (display "(")
    (iter ls 0)
    (display ")")
    (print "")))

; Calculate the sum of a function with inputs from 1 to n.
(define sum
  (lambda (n f)
    (if (> n 0)
      (+ (f n)
         (sum (- n 1) f))
      0)))

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
      ((lambda ()
        (print "abc")
        (qwerty (- count 1)))))))

(define qwerty
  (lambda (count)
    (if (eq? count 0)
      0
      ((lambda ()
        (print "qwerty")
        (abc (- count 1)))))))

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
      ((lambda ()
        (display x) (print " bottles of beer on the wall")
        (display x) (print " bottles of beer")
        (print "Take one down, pass it around")
        (display (- x 1)) (print " bottles of beer on the wall")
        (beer (- x 1))))
      #f)))

; The main function, used as the entry point
(define main
  (lambda ()
	(print "Hello, world!")

    (print "-== Factorial of 6:")
	(print wut)

    (print "-== Squares of numbers from 1 to 30:")
    (psquares 30)))

(main)
