; see http://srfi.schemers.org/srfi-1/srfi-1.html#TheProcedures

(define-syntax iota
  (syntax-rules ()
    ((_ count)
     (gen_range count 0 1))
    ((_ count start)
     (gen_range count start 1))
    ((_ count start step)
     (gen_range count start step))))

(define gen_range
  (lambda (count start step)
    (if (and (not (eq? start 0)) (not (eq? step 1)))
      (iterator (lambda (n) (+ (* n step) start)) count)
    (if (not (eq? start 0))
      (iterator (lambda (n) (+ n start)) count)
    (if (not (eq? step 1))
      (iterator (lambda (n) (* n step)) count)
     else
      (iterator (lambda (n) n) count))))))

(define any
  (lambda (fn xs)
    (member? #t (map fn xs))))
(define ∃ any)

(define every
  (lambda (fn xs)
    (not (member? #f (map fn xs)))))
(define ∀ every)

(define assq
  (lambda (key xs)
	(if (or (null? xs)
			(not (list? (car xs))))
	  #f
	  (if (eq? key (caar xs))
		(car (cdr (car xs)))
		(assq key (cdr xs))))))

(define take)
(define take
  (lambda (x i)
    (if (or (eq? i 0) (null? x))
      '()
     else
      (cons (car x)
            (take (cdr x) (- i 1))))))

(define drop)
(define drop
  (lambda (x i)
    (if (or (eq? i 0) (null? x))
      x
     else
      (drop (cdr x) (- i 1)))))
