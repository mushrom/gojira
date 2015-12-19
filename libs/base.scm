; necessary bits of the language that aren't implemented by the interpreter

; todo: make this simpler
(intern-set 'define
  (syntax-rules ()
    ((_ (funcname args ...) body ...)
     (intern-set 'funcname
        (begin
          (intern-set :mut 'funcname 0)
          (intern-set :mut 'funcname
             (lambda (args ...) body ...))
          funcname)))

    ((_ mutable (funcname args ...) body ...)
     (intern-set mutable 'funcname
        (begin
          (intern-set :mut 'funcname 0)
          (intern-set :mut 'funcname
                      (lambda (args ...) body ...))
          funcname)))

    ((_ (funcname) body ...)
     (intern-set 'funcname
        (begin
          (intern-set :mut 'funcname 0)
          (intern-set :mut 'funcname
                      (lambda () body ...))
          funcname)))

    ((_ mutable (funcname) body ...)
     (intern-set mutable 'funcname
        (begin
          (intern-set :mut 'funcname 0)
          (intern-set :mut 'funcname
                      (lambda () body ...))
          funcname)))

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
    ;((_ ((varname mutable expression) e2 ...) body ...)
    ; (begin
    ;   (intern-set mutable 'varname expression)
    ;   (let (e2 ...) body ...)))

    ((_ ((varname expression) e2 ...) body ...)
     ((lambda ()
       (intern-set 'varname expression)
       (let (e2 ...) body ...))))

    ;((_ ((varname mutable expression)) body ...)
    ; (begin
    ;   (intern-set mutable 'varname expression)
    ;    body ...))

    ((_ ((varname expression)) body ...)
     ((lambda ()
       (intern-set 'varname expression)
        body ...)))))

(define let* let)

(define help "To see the gojira scheme tutorial, visit https://example.com. To see the currently defined variables, try (stacktrace).")

(define (not x)
  (if x
    #f
    #t))

(define = eq?)

(define (<= a b)
  (or
    (< a b)
    (eq? a b)))

(define (>= a b)
  (or
    (> a b)
    (eq? a b)))

(define (caaar xs) (car (car (car xs))))
(define (caadr xs) (car (car (cdr xs))))
(define (caar  xs) (car (car xs)))
(define (cadar xs) (car (cdr (car xs))))
(define (caddr xs) (car (cdr (cdr xs))))
(define (cadr  xs) (car (cdr xs)))
(define (cdaar xs) (cdr (car (car xs))))
(define (cdadr xs) (cdr (car (cdr xs))))
(define (cdar  xs) (cdr (car xs)))
(define (cddar xs) (cdr (cdr (car xs))))
(define (cdddr xs) (cdr (cdr (cdr xs))))
(define (cddr  xs) (cdr (cdr xs)))

(define (print x)
  (display x)
  (newline))

(define (map fn set)
  (if (null? set)
    '()
    (cons
      (fn (car set))
      (map fn (cdr set)))))

(define (member? obj xs)
  (if (not (null? xs))
    (if (eq? obj (car xs))
      #t
      (member? obj (cdr xs)))
    #f))

(define ∈ member?)

(define (append xs obj)
  (if (null? xs)
    obj
    (if (null? (cdr xs))
      (cons (car xs) obj)
      (cons (car xs) (append (cdr xs) obj)))))

(define (length ls)
  (if (null? ls)
    0
    (+ (length (cdr ls)) 1)))

; A basic module system
(define :mut modules '(base))
(define :mut modpath "")

(define (as_modpath name)
    (string-append (string-append modpath name) ".scm"))

(define (import! modname)
  (if (not (member? modname modules))
    (if (load! (as_modpath (symbol->string modname)))
      (begin
        (intern-set! 'modules (cons modname modules))
        #t)
      #f)
    #f))

(define (use! modlist)
  (map import! modlist))
