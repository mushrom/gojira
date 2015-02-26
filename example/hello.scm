;(define-syntax define
(intern-set 'define
  (syntax-rules ()
    ((_ sym def)
     (intern-set 'sym def))
    ((_ sym)
     (intern-set 'sym 0))))

(define print
  (lambda (a)
    (display a)
    (newline)))

(define meh
  (lambda (test)
    ;(intern-set 'bar 'bar)
    (define bar 'bar)
    (lambda (foo)
      (display test)
      (display bar)
      (display foo)
      (newline)))) 

(display "Hello, world!")
(newline)

(display "1 + 2 is ")
;(display (+ 1 2)) (newline)
(print (+ 1 2))
(print ((meh "asdf") ':wut))
