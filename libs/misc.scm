(define (sleep n)
  (if (> n 0)
    (begin
      (intern-sleep)
      (sleep (- n 1)))
    '()))
