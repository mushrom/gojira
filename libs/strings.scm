(define (str-iter str)
  (iterator
    (lambda (n)
      (string-ref str n))
    (string-length str)))

(define (string-concat xs)
  (if (null? xs)
    ""
    (string-append (car xs)
                   (string-concat (cdr xs)))))

(define (concat :rest strs)
  (string-concat strs))

(define string+ concat)
