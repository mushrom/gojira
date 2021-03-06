; see http://srfi.schemers.org/srfi-1/srfi-1.html#TheProcedures

(define-syntax :builtin iota
  (syntax-rules ()
    ((_ count)
     (gen_range count 0 1))
    ((_ count start)
     (gen_range count start 1))
    ((_ count start step)
     (gen_range count start step))))

(define :builtin (gen_range count start step)
  (define (gen_iter i sum)
    (if (< i count)
      (cons sum (gen_iter (+ i 1) (+ sum step)))
      '()))

  (gen_iter 0 start))

(define :builtin (append xs obj)
  (if (null? xs)
    obj
    (if (null? (cdr xs))
      (cons (car xs) obj)
      (cons (car xs) (append (cdr xs) obj)))))

(define :builtin (reverse xs)
  (if (or (null? xs)
          (null? (cdr xs)))
    xs
    (append (reverse (cdr xs)) (list (car xs)))))

(define :builtin (length ls)
  (if (null? ls)
    0
    (+ (length (cdr ls)) 1)))

(define :builtin (assq key xs)
  (if (or (null? xs)
          (not (list? (car xs))))
    #f
    (if (eq? key (caar xs))
      (car (cdr (car xs)))
      (assq key (cdr xs)))))

(define :builtin (take x i)
  (if (or (eq? i 0) (null? x))
    '()
    else
    (cons (car x)
          (take (cdr x) (- i 1)))))

(define :builtin (drop x i)
  (if (or (eq? i 0) (null? x))
    x
    else
    (drop (cdr x) (- i 1))))

(define :builtin (equal? xs ys)
    (if (list? xs)
      (or (and (null? xs) (null? ys))
          (and
            (and
              (and
                (not (null? xs))
                (not (null? ys)))
              (equal? (car xs) (car ys)))
            (equal? (cdr xs) (cdr ys))))
     else
      (eq? xs ys)))

(define :builtin (map fn set)
  (if (null? set)
    '()
    (cons
      (fn (car set))
      (map fn (cdr set)))))

(define :builtin (for-each f xs)
  (if (null? xs)
    '()
    (begin
      (f (car xs))
      (for-each f (cdr xs)))))

(define :builtin (list-replace xs old new)
  (if (null? xs)
    '()
    (if (eq? (car xs) old)
      (cons new (list-replace (cdr xs) old new))
      else
      (cons (car xs) (list-replace (cdr xs) old new)))))

(define :builtin (delim xs token)
  (if (null? xs)
    '()
    (if (eq? (car xs) token)
      '()
      (cons (car xs) (delim (cdr xs) token)))))

(define :builtin (after xs token)
  (if (null? xs)
    '()
    (if (eq? (car xs) token)
      (cdr xs)
      else
      (after (cdr xs) token))))

(define :builtin (list-split xs token)
  (if (null? xs)
    '()
    (cons (delim xs token) (list-split (after xs token) token))))

(define :builtin (list-ref xs n)
  (if (null? xs)
    '()
    (if (eq? n 0)
      (car xs)
      (list-ref (cdr xs) (- n 1)))))

(define :builtin (filter sieve xs)
  (if (null? xs)
    '()
    (if (sieve (car xs))
      (cons (car xs) (filter sieve (cdr xs)))
      (filter sieve (cdr xs)))))

(define :builtin (zip xs ys)
  (if (or (null? xs)
          (null? ys))
    '()
   else
    (cons (list (car xs) (car ys))
          (zip  (cdr xs) (cdr ys)))))

(define :builtin (where pred funct xs)
  (cond 
    ((null? xs) '())
    ((pred xs) (cons (funct (car xs))
                     (where pred funct (cdr xs))))
    (true (cons (car xs)
                (where pred funct (cdr xs))))))

(define :builtin (safecar xs)
  (if (null? xs)
    '()
    (car xs)))

(define :builtin (safecdr xs)
  (if (null? xs)
    '()
    (cdr xs)))

(define-syntax :builtin with
  (syntax-rules (as)
    ((_ xs as (a1 a2 ...) body ...)
     (let ((a1 (safecar xs)))
       (with (safecdr xs) as (a2 ...) body ...)))

    ((_ xs as () body ...)
     (begin body ...))))

(define-syntax :builtin for
  (syntax-rules (in)
    ((_ (args ...) in somelist body ...)
     (map
       (lambda (foobar)
         (with foobar as (args ...)
               body ...))
       somelist))

    ((_ varname in somelist body ...)
     (map
       (lambda (varname)
         body ...)
       somelist))))

(define-syntax :builtin for-var
  (syntax-rules (in)
    ((_ (args ...) in somelist body ...)
     (for-each
       (lambda (foobar)
         (with foobar as (args ...)
               body ...))
       somelist))

    ((_ varname in somelist body ...)
     (for-each
       (lambda (varname)
         body ...)
       somelist))))

(define :builtin (display-list :rest xs)
  (map display xs))

(define :builtin (any fn xs)
  (member? #t (map fn xs)))

(define :builtin ∃ any)

(define :builtin (every fn xs)
  (not (member? #f (map fn xs))))

(define :builtin ∀ every)
