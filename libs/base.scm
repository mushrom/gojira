; necessary bits of the language that aren't implemented by the interpreter
(intern-set 'define
  (syntax-rules ()
    ((_ mutable (funcname args ...) body ...)
     (intern-set mutable 'funcname
                    (lambda (args ...) body ...)))

    ((_ (funcname args ...) body ...)
     (intern-set 'funcname
                    (lambda (args ...) body ...)))

    ((_ mutable (funcname) body ...)
     (intern-set mutable 'funcname
                    (lambda () body ...)))

    ((_ (funcname) body ...)
     (intern-set 'funcname
                    (lambda () body ...)))

    ((_ mutable sym def)
     (intern-set mutable 'sym def))

    ((_ sym def)
     (intern-set 'sym def))

    ((_ sym)
     (intern-set 'sym 0))))

(define define-syntax define)

(define-syntax if
  (syntax-rules ()
    ((_ condition a else b)
     ((condition (lambda () a)
                 (lambda () b))))
    ((_ condition a b)
     ((condition (lambda () a)
                 (lambda () b))))))

(define help "To see the gojira scheme tutorial, visit https://example.com. To see the currently defined variables, try (stacktrace).")

(define not
  (lambda (x)
    (if x
      #f
      #t)))

; TODO: Fix symbol clashes between procedures and macro expansion
(define-syntax or
  (syntax-rules ()
    ((_ _op1_ _op2_ more ...)
     (if _op1_
       #f
       (or _op2_ more ...)))

    ((_ _op1_ _op2_)
     (if _op1_
       #t
       (if _op2_
         #t
         #f)))))

(define-syntax and
  (syntax-rules ()
    ((_ _op1_ _op2_ more ...)
     (if _op1_
       (and _op2_ more ...)
       #f))

    ((_ _op1_ _op2_)
     (if _op1_
       (if _op2_
         #t
         #f)
       #f))))

(define-syntax cond
  (syntax-rules ()
    ((_ (pred body ...) more ...)
     (if pred
       (begin body ...)
       (cond more ...)))

    ((_ (pred body ...))
     (if pred
       (begin body ...)
       #f))))

(define-syntax let
  (syntax-rules ()
    ((_ ((varname expression) e2 ...) body ...)
     (begin
       (intern-set 'varname expression)
       (let (e2 ...) body ...)))

    ((_ ((varname expression)) body ...)
     (begin
       (intern-set 'varname expression)
        body ...))))

(define let* let)

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
    (display x)
	(newline)))

(define map
  (lambda (fn set)
    (if (null? set)
      '()
      (cons
        (fn (car set))
        (map fn (cdr set))))))

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

(define length
  (lambda (ls)
    (if (null? ls)
      0
      (seq (length (cdr ls))))))

; A basic module system
(define :mut modules '("base"))
(define :mut modpath "")

(define as_modpath
  (lambda (name)
    (string-append (string-append modpath name) ".scm")))

(define import!
  (lambda (modname)
    (if (not (member? modname modules))
      (if (load! (as_modpath (symbol->string modname)))
        (begin
          (intern-set! 'modules (cons modname modules))
          #t)
        #f)
      #f)))

(define use!
  (lambda (modlist)
    (map import! modlist)))
