; symbols
;; => a
(write 'a)
(newline)

;; => abc
(write 'abc)
(newline)

;; => #t
(write (eq? 'abc 'abc))
(newline)

;; => #f
(write (eq? 'abc 'def))
(newline)

;; => zeblarg
(write 'zeblarg)
(newline)

;; => fooba&r-_baz?
(write 'fooba&r-_baz?)
(newline)

;; => :asdf
(write :asdf)
(newline)

;; => #t
(write (eq? :asdf ':asdf))
(newline)
