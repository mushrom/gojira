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

(define string+ string-append)

(define concat
  (syntax-rules ()
    ((_ strings ...)
     (string-concat (list strings ...)))))
