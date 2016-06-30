;; => #(a b c 1 2 3 :foo :bar (baz :que))
(write #(a b c 1 2 3 :foo :bar (baz :que)))
(newline)

;; => #t
(write (vector? #(lorem ipsum)))
(newline)

;; => #f
(write (vector? '(this is probably not a vector)))
(newline)

(write ((lambda (foo)
;; => :foo
   (write (vector-ref foo 0))
   (newline)

;; => wut
   (write (vector-ref foo 2))
   (newline)

;; => 133
   (write (+ (vector-ref foo 1) 10))
   (newline)
   
   foo)

;; => #(:foo 123 wut)
 #(:foo 123 wut)))
(newline)

((lambda (foo)
;; => #(:foo :bar :baz)
   (write foo)
   (newline)

;; => #(:foo :bar (a b c))
   (write (vector-set! foo 2 '(a b c)))
   (newline)

;; => #(:foo :bar (a b c))
   (write foo)
   (newline))
 #(:foo :bar :baz))

; test for bug fix at 432eca7e4
;; => #(a b c d)
(print (list->vector '(a b c d)))
