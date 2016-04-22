;; => #t
(print (eq? (+ (values 1 2 3 4))
            (+ 1 2 3 4)))

;; => foo
(print (begin (values 'foo 'bar 'baz)))
