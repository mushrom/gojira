; stress test for scoping rules
(define blarg
  (lambda (x)
    (define :mut y (* x 2))
    (write y)
    (newline)))

(blarg 5)
