; lists
;; => ()
(write '())
(newline)

;; => (a b c)
(write '(a b c))
(newline)

;; => (testing this)
(write '(testing this))
(newline)

;; => (foo (bar ((baz faz)) hello) cool world)
(write '(foo (bar ((baz faz)) hello) cool world))
(newline)

;; => (1 2 3 (4 5 6) qwerty "foo")
(write (list 1 2 (+ 1 2) (list 4 5 6) 'qwerty "foo"))
(newline)
