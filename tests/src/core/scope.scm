; stress test for scoping rules
;; => 10
(define blarg
  (lambda (x)
    (define :mut y (* x 2))
    (write y)
    (newline)))

(blarg 5)
