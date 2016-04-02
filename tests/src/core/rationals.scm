; check for correctness at most basic level
;; => 1/2
(write (/ 1 2))
(newline)

;; => 1/10
(write (/ 10 100))
(newline)

; check for lexer correctness
;; => 1/2
(write 1/2)
(newline)

;; => 3/4
(write 30/40)
(newline)

;; => -1/2
(write -1/2)
(newline)

;; => -4/5
(write 4/-5)
(newline)

; check addition
;; => 1
(write (+ 2/3 1/3))
(newline)

;; => 11/6
(write (+ 2/4 1/3 1))
(newline)

;; => -1/6
(write (+ -2/4 1/3))
(newline)

; check subtraction
;; => 11/3
(write (- 4 1/3))
(newline)

;; => 1
(write (- 6/4 1/2))
(newline)

;; => -2
(write (- -6/4 1/2))
(newline)

; check multiplication
;; => 1/9
(write (* 1/3 1/3))
(newline)

;; => -1
(write (* 4 -1/4))
(newline)

;; => -1/5
(write (* -1/2 2/5))
(newline)

; check division
;; => 1
(write (/ 1/3 1/3))
(newline)

;; => 4
(write (/ 1 1/4))
(newline)

;; => 1/2
(write (/ 5 10))
(newline)
