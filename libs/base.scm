; program to test the interpreter
; (proper test suite coming soon(!))

; quick unicode reference:
;   ¬ = U+ac
;   λ = U+3bb
;   ∀ = U+2200
;   ∁ = U+2201
;   ∂ = U+2202
;   ∃ = U+2203
;   ∈ = U+2208
;   ∑ = U+2211
;   ∧ = U+2227
;   ∨ = U+2228


(define-syntax define
  (syntax-rules ()
    ((_ sym def)
     (intern-set 'sym def))
    ((_ sym)
     (intern-set 'sym 0))))

; this will do until proper elipsis expansion is implemented...
(define-syntax begin
  (syntax-rules ()
	((_ expr moar)
	 ((lambda () expr (begin moar))))
	((_ expr)
	 expr)
	((_) #f)))

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

(define >=
  (lambda (a b)
	(or
	  (> a b)
	  (eq? a b))))

(define caar
  (lambda (x)
	(car (car x))))

(define caaar
  (lambda (x)
	(car (caar x))))

(define print
  (lambda (x)
    (if (list? x)
      (pprint-list x)
      (display x))
	(newline)))

(define map
  (lambda (func set)
    (if (null? set)
      '()
      (cons
        (func (car set))
        (map func (cdr set))))))

(define member?
  (lambda (obj xs)
    (if (not (null? xs))
      (if (eq? obj (car xs))
        #t
        (member? obj (cdr xs)))
      #f)))
(define ∈ member?)

(define append
  (lambda (xs obj)
    (if (null? xs)
      obj
    (if (null? (cdr xs))
      (cons (car xs) obj)
      (cons (car xs) (append (cdr xs) obj))))))

(define pprint-list
  (lambda (ls)
    (define iter
      (lambda (x indent)
        (if (null? x)
          x
          (if (list? (car x))
            ((lambda ()
               (display "(")
               (iter (car x) (+ indent 4))
               (if (null? (cdr x))
                  (display ")")
                  (display ") "))
               (iter (cdr x) indent)))
            ((lambda ()
              ;(for indent (lambda (y) (display " ")))
              (display (car x))
              (if (null? (cdr x))
                 #f
                 (display " "))
              (iter (cdr x) indent)))))))

    (display "(")
    (iter ls 0)
    (display ")")))
