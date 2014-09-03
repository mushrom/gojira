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

; Calculate the sum of a function with inputs from 1 to n.
(define sum
  (lambda (n f)
    (if (>= n 1)
      (+ (f n)
         (sum (- n 1) f))
      0)))

; Calculate the sum of a function with inputs from k to n.
(define sigma
  (lambda (n k f)
    (if (>= n k)
      (+ (f n)
         (sigma (- n 1) k f))
      0)))

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
