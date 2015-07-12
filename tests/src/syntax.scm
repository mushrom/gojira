(define-syntax asdf
  (syntax-rules ()
    ((_)     1)
    ((_ a)   2)
    ((_ a b) (+ a b))))

(display (asdf))
(newline)

(display (asdf 1))
(newline)

(display (asdf 1 2))
(newline)

(display (asdf (asdf 1 2) 3))
(newline)
