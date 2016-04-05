(define :builtin (string-concat xs)
  (if (null? xs)
    ""
    (string-append (car xs)
                   (string-concat (cdr xs)))))

(define :builtin (concat :rest strs)
  (string-concat strs))

(define :builtin string+ concat)

(define :builtin (string->list str)
  (map (lambda (n)
         (string-ref str n))
       (iota (string-length str))))
