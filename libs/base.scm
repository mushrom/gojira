; necessary bits of the language that aren't implemented by the interpreter

; todo: make this simpler
(intern-set :builtin 'define
  (syntax-rules ()
    ((_ (funcname args ...) body more ...)
     (intern-set 'funcname
        ((lambda (funcname)
           (intern-set :mut 'funcname
                       (lambda (args ...) body more ...))
           funcname)
         #f)))

    ((_ mutable (funcname args ...) body more ...)
     (intern-set mutable 'funcname
        ((lambda (funcname)
           (intern-set :mut 'funcname
                       (lambda (args ...) body more ...))
           funcname)
         #f)))

    ((_ mutable sym def)
     (intern-set 'mutable 'sym def))

    ((_ sym def)
     (intern-set 'sym def))

    ((_ sym)
     (intern-set 'sym #f))))

(define :builtin define-syntax define)

(define-syntax :builtin set!
  (syntax-rules ()
    ((_ varname value)
     (intern-set! 'varname value))))

(define-syntax :builtin if
  (syntax-rules ()
    ((_ condition a else b)
     ((condition (lambda () a)
                 (lambda () b))))
    ((_ condition a b)
     ((condition (lambda () a)
                 (lambda () b))))))

(define-syntax :builtin begin
  (syntax-rules ()
    ((_ body more ...)
     ((lambda ()
        body more ...)))))

(define-syntax :builtin or
  (syntax-rules ()
    ((_ e1 e2 e3 ...)
     (if e1
       #t
       (or e2 e3 ...)))

    ((_ e1) e1)))

(define-syntax :builtin and
  (syntax-rules ()
    ((_ e1 e2 e3 ...)
     (if e1
       (and e2 e3 ...)
       #f))

    ((_ e1) e1)))

(define-syntax :builtin cond
  (syntax-rules ()
    ((_ (pred body ...) more ...)
     ((pred
       (lambda () body ...)
       (lambda () (cond more ...)))))

    ((_) ; cond didn't match anything, what do?
     #f)))

(define-syntax :builtin let
  (syntax-rules ()
    ((_ ((varname expression) e2 ...) body ...)
     ((lambda (varname)
       (let (e2 ...) body ...))
      expression))

    ((_ () body ...)
     ((lambda () body ...)))

    ((_ proc-id (vardefs ...) body ...)
     (let (vardefs ...)
       (define (proc-id) body ...)
       (proc-id)))))

(define :builtin let* let)

(define-syntax :builtin call-with-current-continuation
  (syntax-rules ()
    ((_ fn)
     (fn (intern-get-continuation)))))

(define :builtin call/cc call-with-current-continuation)

(define :builtin (values :rest things)
  (call/cc
    (lambda (c)
      (apply c things))))

(define-syntax :builtin when
  (syntax-rules ()
    ((_ condition body ...)
     (if condition
       (begin
         body ...)
       '()))))

(define-syntax :builtin unless
  (syntax-rules ()
    ((_ condition body ...)
     (if condition
       '()
       (begin body ...)))))

(define-syntax :builtin while
  (syntax-rules ()
    ((_ condition body ...)
     (let loopy ()
       (when condition
         (begin body ...)
         (loopy))))))

(define :builtin (ident x) x)

(define :builtin (intern-quasiquote expr)
  (cond
    ((null? expr) '())

    ((null? (car expr))
     (cons '() (intern-quasiquote (cdr expr))))

    ((and (list? (car expr))
          (eq? (caar expr) 'unquote))
     (cons (eval (cons 'ident (cdar expr)))
           (intern-quasiquote (cdr expr))))

    ((list? (car expr))
     (cons (intern-quasiquote (car expr))
           (intern-quasiquote (cdr expr))))

    (true (cons (car expr)
          (intern-quasiquote (cdr expr))))))

(define-syntax :builtin quasiquote
  (syntax-rules ()
    ((_ expr)
     (intern-quasiquote 'expr))))

(define help "To see the gojira scheme tutorial, visit https://example.com. To see the currently defined variables, try (stacktrace).")

(define :builtin (not x)
  (if x
    #f
    #t))

(define :builtin = eq?)

(define :builtin (<= a b)
  (or
    (< a b)
    (eq? a b)))

(define :builtin (>= a b)
  (or
    (> a b)
    (eq? a b)))

(define :builtin (caaar xs) (car (car (car xs))))
(define :builtin (caadr xs) (car (car (cdr xs))))
(define :builtin (caar  xs) (car (car xs)))
(define :builtin (cadar xs) (car (cdr (car xs))))
(define :builtin (caddr xs) (car (cdr (cdr xs))))
(define :builtin (cadr  xs) (car (cdr xs)))
(define :builtin (cdaar xs) (cdr (car (car xs))))
(define :builtin (cdadr xs) (cdr (car (cdr xs))))
(define :builtin (cdar  xs) (cdr (car xs)))
(define :builtin (cddar xs) (cdr (cdr (car xs))))
(define :builtin (cdddr xs) (cdr (cdr (cdr xs))))
(define :builtin (cddr  xs) (cdr (cdr xs)))

(define :builtin (print x)
  (display x)
  (newline))

(define :builtin (member? obj xs)
  (if (not (null? xs))
    (if (eq? obj (car xs))
      #t
      (member? obj (cdr xs)))
    #f))

(define :builtin ∈ member?)

; A basic module system
(define :mut modules '(base))
(define :mut modpath "")

(define :builtin (as_modpath name)
    (string-append (string-append modpath name) ".scm"))

(define :builtin (import! modname)
  (if (not (member? modname modules))
    (if (load! (as_modpath (symbol->string modname)))
      (begin
        (set! modules (cons modname modules))
        #t)
      #f)
    #f))

(define-syntax :builtin use
  (syntax-rules ()
    ((_ mod)
     (import! 'mod))))

(define modpath gojira-modpath)

(import! 'lists)
(import! 'strings)
