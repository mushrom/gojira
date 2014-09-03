; program to test the interpreter
; (proper test suite coming soon(!))

(define-syntax define
  (syntax-rules ()
    ((_ sym def)
     (intern-set 'sym def))
    ((_ sym)
     (intern-set 'sym 0))))

(define help "To see the gojira scheme tutorial, visit http://example.com. To see the currently defined variables, try (stacktrace).")

(define not
  (lambda (x)
    (if x
      #f
      #t)))

; TODO: Fix symbol clashes between procedures and macro expansion
(define-syntax or
  (syntax-rules ()
    ((_ _op1_ _op2_)
     (if _op1_
       #t
       (if _op2_
         #t
         #f)))))

(define-syntax and
  (syntax-rules ()
    ((_ _op1_ _op2_)
     (if _op1_
       (if _op2_
         #t
         #f)
       #f))))

(define = eq?)

(define <=
  (lambda (a b)
	(or
	  (< a b)
	  (eq? a b))))

(define caar
  (lambda (x)
	(car (car x))))

(define caaar
  (lambda (x)
	(car (caar x))))

(define print
  (lambda (x)
	(display x)
	(newline)))
