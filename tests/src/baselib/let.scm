;; => 'wut
(let ()
  (write 'wut)
  (newline))

;; => 10
(let ((x 10))
  (write x)
  (newline))

;; => 100
;; => 400
(let ((x 10)
      (y 20))
  (write (* x x))
  (newline)

  (write (* y y))
  (newline))

;; => 0
;; => 1
;; => 2
;; => 3
;; => 4
(let foo ((i   0)
          (lim 5))
  (if (< i lim)
    (begin
      (print i)
      (set! i (+ i 1))
      (foo))
    '()))
