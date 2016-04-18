;; => test
(write ((lambda () 'test)))
(newline)

;; => foothing
(((lambda (x)
    (lambda (y)
      (x y)))
  (lambda (x)
    (write 'foo)
    (write x)
    (newline)))
 'thing)

;; => 15129
(write ((lambda (foo)
            (* foo foo)) 123))
(newline)
