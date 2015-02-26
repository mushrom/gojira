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

    ;(define iter
    (intern-set 'iter
      (lambda (i accum xs)
        (if (not (eq? i count))
          (cons accum
            (iter
              (+ i 1)
              (+ accum step)
              '()))
          '())))

    (iter 0 start '())))

(define old_gen_range
  (lambda (count start step)

    (define iter
      (lambda (i sum xs)
        (if (not (eq? i count))
          (iter
            (+ i 1)
            (+ sum step)
            (append xs (cons sum '())))
          xs)))

    (iter 0 start '())))

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
