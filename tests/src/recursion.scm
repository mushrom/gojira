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

(recurse)
