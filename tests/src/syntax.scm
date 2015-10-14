(define-syntax asdf
  (syntax-rules ()
    ((_)     1)
    ((_ a)   2)
    ((_ a b) (+ a b))))

(write (asdf))
(newline)

(write (asdf 1))
(newline)

(write (asdf 1 2))
(newline)

(write (asdf (asdf 1 2) 3))
(newline)
