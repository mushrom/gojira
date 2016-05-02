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

(define :builtin (number->charlist num)
  (define (iter num xs)
    (if (> num 0)
      (let ((char
              (assq (modulo num 10)
                    '((0 #\0) (1 #\1) (2 #\2) (3 #\3)
                      (4 #\4) (5 #\5) (6 #\6) (7 #\7)
                      (8 #\8) (9 #\9)))))

        (iter (floor (/ num 10)) (cons char xs)))
     else
       xs))

  (cond
    ((> num 0)
     (iter num '()))

    ((< num 0)
     (cons #\- (iter (* -1 num) '())))

    (true
      (list #\0))))

(define :builtin (number->string num)
  (list->string (number->charlist num)))

; todo: handle negative numbers here
(define :builtin string-number-map
  (hashmap "0" 0 "1" 1 "2" 2
           "3" 3 "4" 4 "5" 5
           "6" 6 "7" 7 "8" 8
           "9" 9))

(define :builtin hex-number-map
  (hashmap "0" 0  "1" 1  "2" 2  "3" 3
           "4" 4  "5" 5  "6" 6  "7" 7
           "8" 8  "9" 9  "a" 10 "b" 11
           "c" 12 "d" 13 "e" 14 "f" 15))

(define :builtin (charlist->number xs)
  (let ((c [string-number-map (list->string (list (car xs)))]))
    (if (not (eq? c #f))
      (if (null? (cdr xs))
        c
       else
        (+ c (* 10 (charlist->number (cdr xs)))))
     else 0)))

(define :builtin (hex-charlist->number xs)
  (let ((c [hex-number-map (list->string (list (car xs)))]))
    (if (not (eq? c #f))
      (if (null? (cdr xs))
        c
       else
        (+ c (* 16 (hex-charlist->number (cdr xs)))))
     else 0)))

(define :builtin (string->number str)
  (charlist->number (reverse (string->list str))))

(define :builtin (hex-string->number str)
  (hex-charlist->number (reverse (string->list str))))
