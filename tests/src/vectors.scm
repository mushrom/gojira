(write #(a b c 1 2 3 :foo :bar (baz :que)))
(newline)

(write (vector? #(lorem ipsum)))
(newline)

(write (vector? '(this is probably not a vector)))
(newline)

(write ((lambda (foo)
   (write (vector-ref foo 0))
   (newline)

   (write (vector-ref foo 2))
   (newline)

   (write (+ (vector-ref foo 1) 10))
   (newline)
   
   foo)
 #(:foo 123 wut)))
(newline)

((lambda (foo)
   (write foo)
   (newline)

   (write (vector-set! foo 2 '(a b c)))
   (newline)

   (write foo)
   (newline))
 #(:foo :bar :baz))
