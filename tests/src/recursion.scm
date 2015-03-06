(intern-set 'if
  (syntax-rules ()
    ((_ condition a b)
     ((condition (lambda () a)
                 (lambda () b))))))

(intern-set 'recurse-func
  (lambda (x)
    (display x)
    (newline)
    (if (< x 10)
      (recurse-func (+ x 1))
      '())))

(intern-set 'recurse
  (lambda ()
    (recurse-func 0))) 

(intern-set 'qwerty
  (lambda (num)
    (display 'qwerty)
    (newline)

    (if (> num 0)
      (abc (- num 1))
      '())))

(intern-set 'abc
  (lambda (num)
    (display 'abc)
    (newline)

    (if (> num 0)
      (qwerty (- num 1))
      '())))

(recurse)
(abc 10)
