; stress test for scoping rules

(define-syntax define
  (syntax-rules ()
    ((_ sym def)
     (intern-set 'sym def))
    ((_ sym)
     (intern-set 'sym 0))))

(define blarg
  (lambda (x)
    (define x 10)
    (display x)
    (newline)))

(blarg 5)
