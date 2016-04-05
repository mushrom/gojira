; Square a number
(define :builtin (square x)
  (* x x))

(define :builtin even?
  (lambda (n)
    (eq? (modulo n 2) 0)))

(define :builtin odd?
  (lambda (n)
    (not (even? n))))

(define :builtin pi 3.1415926535)
