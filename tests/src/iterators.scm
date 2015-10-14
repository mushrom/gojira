(import! 'lists)

(define infinite-thing
  (iterator (lambda (x) x)))

(write (car infinite-thing))
(newline)

(write (caddr infinite-thing))
(newline)

(foreach (take infinite-thing 10)
    (lambda (x)
      (write x)
      (newline)))

(write (map (lambda (n) (* n n))
            (iterator
              (lambda (foo) foo)
              10)))
(newline)
