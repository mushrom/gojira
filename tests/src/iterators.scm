(import! 'lists)

(define infinite-thing
  (iterator (lambda (x) x)))

(write (car infinite-thing))
(newline)

(write (caddr infinite-thing))
(newline)

(for-each (lambda (x)
            (write x)
            (newline))
          (take infinite-thing 10))

(write (map (lambda (n) (* n n))
            (iterator
              (lambda (foo) foo)
              10)))
(newline)
