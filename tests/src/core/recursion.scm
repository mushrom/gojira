(intern-set 'recurse-func
  (lambda (x)
    (write x)
    (newline)
    (if (< x 10)
      (recurse-func (+ x 1))
      '())))


(intern-set 'recurse
  (lambda ()
    (recurse-func 0))) 

(intern-set 'qwerty
  (lambda (num)
    (write 'qwerty)
    (newline)

    (if (> num 0)
      (abc (- num 1))
      '())))

(intern-set 'abc
  (lambda (num)
    (write 'abc)
    (newline)

    (if (> num 0)
      (qwerty (- num 1))
      '())))

;; => 0
;; => 1
;; => 2
;; => 3
;; => 4
;; => 5
;; => 6
;; => 7
;; => 8
;; => 9
;; => 10
(recurse)

;; => abc
;; => qwerty
;; => abc
;; => qwerty
;; => abc
;; => qwerty
;; => abc
;; => qwerty
;; => abc
;; => qwerty
;; => abc
(abc 10)
