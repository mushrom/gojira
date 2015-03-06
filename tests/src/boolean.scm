(intern-set 'if
  (syntax-rules ()
    ((_ condition a b)
     ((condition (lambda () a)
                 (lambda () b))))))

(display (#t 'true 'false))
(newline)

(display (if (< 1 2) #t #f))
(newline)

(display (if (> 3 2)
           (if (< 2 1)
             #t
             #f)
           #f))
(newline)