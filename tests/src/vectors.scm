(display #(a b c 1 2 3 :foo :bar (baz :que)))
(newline)

(display (vector? #(lorem ipsum)))
(newline)

(display (vector? '(this is probably not a vector)))
(newline)

(display ((lambda (foo)
   (display (vector-ref foo 0))
   (newline)

   (display (vector-ref foo 2))
   (newline)

   (display (+ (vector-ref foo 1) 10))
   (newline)
   
   foo)
 #(:foo 123 wut)))
(newline)

((lambda (foo)
   (display foo)
   (newline)

   (display (vector-set! foo 2 '(a b c)))
   (newline)

   (display foo)
   (newline))
 #(:foo :bar :baz))
