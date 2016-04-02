; check for basic correctness
;; => 0.1
(write 0.1)
(newline)

;; => 0.1
(write .10)
(newline)

;; => 0.012
(write 1.2e-2)
(newline)

;; => 49000
(write 4.9e4)
(newline)

; operator checks
;; => 1
(write (* (/ 1 3.0) 3))
(newline)

;; => #t
(let ((foo (/ 1 3.0)))
  (write (and (< foo 0.33334)
              (> foo 0.33333))))
(newline)

;; => 0.5
(write (- 1 .5))
(newline)

;; => #t
(let ((foo (+ .5 (/ 1 3.0))))
  (write (and (< foo 0.83334)
              (> foo 0.83333))))
(newline)
