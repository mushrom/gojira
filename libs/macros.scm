(define (wut foo)
  (quasiquote (1 2 (print (unquote foo)) 4 5)))

(define (meanwhile condition body)
  ((eval
    (quasiquote
      (lambda ()
        (let ((loop (lambda ()
                      (cond
                        ((unquote condition)
                         (begin
                           (unquote body)
                           (loop)))))))
          (loop)))))))

(define :mut i 0)

(meanwhile '(< i 10)
  '(begin
     (print i)
     (define i (+ i 1))))
