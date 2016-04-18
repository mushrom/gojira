; test program for libs/base.scm

;; => (this is a variable eh)
(define testing '(this is a variable eh))
(write testing)
(newline)

(define (foo bar)
  (write bar)
  (newline))

;; => 100
(foo (* 10 10))

(define meh
  (lambda (x)
    (* x x)))

;; => 400
(foo (meh 20))

(define :mut a-sum 0)

(define (another-test x)
  (define (thing asdf)
    (set! a-sum (+ a-sum asdf x))
    a-sum)
  
  (thing (meh x)))

;; => 240
(foo (another-test 15))

;; => 350
(foo (another-test 10))

(define (test-fact n)
  (if (> n 1)
    (* n (test-fact (- n 1)))
    1))

;; => 720
(foo (test-fact 6))
