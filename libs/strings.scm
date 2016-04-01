(define (string-concat xs)
  (if (null? xs)
    ""
    (string-append (car xs)
                   (string-concat (cdr xs)))))

(define (concat :rest strs)
  (string-concat strs))

(define string+ concat)

(define (string->list str)
  (map (lambda (n)
         (string-ref str n))
       (iota (string-length str))))
