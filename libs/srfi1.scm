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

    (define iter
      (lambda (i sum xs)
        (if (not (eq? i count))
          (iter
            (+ i 1)
            (+ sum step)
            (append xs (cons sum '())))
          xs)))

    (iter 0 start '())))
