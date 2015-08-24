(import! 'lists)

(define infinite-thing
  (iterator (lambda (x) x)))

(print (car infinite-thing))
(print (caddr infinite-thing))

(foreach (take infinite-thing 10) print)

(print (map (lambda (n) (* n n))
            (iterator
              (lambda (foo) foo)
              10)))
