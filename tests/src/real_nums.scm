; check for basic correctness
(write 0.1)
(newline)

(write .10)
(newline)

(write 1.2e-2)
(newline)

(write 4.9e4)
(newline)

; operator checks
(write (* (/ 1 3.0) 3))
(newline)

(let ((foo (/ 1 3.0)))
  (write (and (< foo 0.33334)
              (> foo 0.33333))))
(newline)

(write (- 1 .5))
(newline)

(let ((foo (+ .5 (/ 1 3.0))))
  (write (and (< foo 0.83334)
              (> foo 0.83333))))
(newline)
