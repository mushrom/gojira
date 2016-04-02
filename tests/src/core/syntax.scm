(define-syntax asdf
  (syntax-rules ()
    ((_)     1)
    ((_ a)   2)
    ((_ a b) (+ a b))))

;; => 1
(write (asdf))
(newline)

;; => 2
(write (asdf 1))
(newline)

;; => 3
(write (asdf 1 2))
(newline)

;; => 6
(write (asdf (asdf 1 2) 3))
(newline)
