; check for correctness at most basic level
(write (/ 1 2))
(newline)

(write (/ 10 100))
(newline)

; check for lexer correctness
(write 1/2)
(newline)

(write 30/40)
(newline)

(write -1/2)
(newline)

(write 4/-5)
(newline)

; check addition
(write (+ 2/3 1/3))
(newline)

(write (+ 2/4 1/3 1))
(newline)

(write (+ -2/4 1/3))
(newline)

; check subtraction
(write (- 4 1/3))
(newline)

(write (- 6/4 1/2))
(newline)

(write (- -6/4 1/2))
(newline)

; check multiplication
(write (* 1/3 1/3))
(newline)

(write (* 4 -1/4))
(newline)

(write (* -1/2 2/5))
(newline)

; check division
(write (/ 1/3 1/3))
(newline)

(write (/ 1 1/4))
(newline)

(write (/ 5 10))
(newline)
