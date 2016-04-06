(define (foo return)
  (return 123)
  321)

;; => 321
(print (foo ident))

;; => 123
(print (call/cc foo))

;; => 0
;; => 1
;; => 2
;; => 3
;; => 4
(define i 0)
(define (thing return)
  (if (< i 5)
    (begin
      (print i)
      (set! i (+ i 1))
      (thing return))

   else
    (return i)))

;; => 5
(print (call/cc thing))

(define asdf)
(define (another-test)
  (+ 1 (* 2 3)
     (call/cc
       (lambda (c)
         (set! asdf c)
         4))))

;; => 11
(print (another-test))

;; => 17
(print (asdf 10))

;; => 17
(print (asdf 10))

;; => 130
(print (foo asdf))
