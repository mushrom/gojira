(define (read-line)
  (let ((c (read-char)))
    (if (eq? c #\newline)
      '()
      (cons c (read-line)))))

(define (prompt str)
  (display str)
  (read-line))

(define (yes-or-no str)
  (display str)
  (display " (yes/no) > ")

  (define (yes-iter)
    (let ((temp (list->string (read-line))))
      (cond
        ((eq? temp "yes")
         #t)

        ((eq? temp "no")
         #f)

        (true
          (display "please enter \"yes\" or \"no\" > ")
          (yes-iter)))))
  
  (yes-iter))

(define (y-or-n str)
  (display str)
  (display " (y/n) > ")

  (define (yes-iter)
    (let ((temp (list->string (read-line))))
      (cond
        ((eq? temp "y")
         #t)

        ((eq? temp "n")
         #f)

        (true
          (display "please enter \"y\" or \"n\" > ")
          (yes-iter)))))
  
  (yes-iter))

(define (select-item str items)
  (let* ((indexes (concat "abcdefghijklmnopqrstuvwxyz"
                          "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                          "0123456789"))
         (choices (zip (str-iter indexes) items))
         (choice-range (list->string
                         (list
                           (string-ref indexes 0)
                           #\-
                           (string-ref indexes
                                       (- (length items) 1))))))

    (for-each
      (lambda (foo)
        (display-list "| " (car foo) ": " (cadr foo) #\newline))
      choices)

    (display-list str " (" choice-range ") > ")

    (define (select-loop)
      (let* ((temp (read-line))
             (index (filter
                      (lambda (x)
                        (equal? (list (car x)) temp))
                      choices)))

        (if (not (null? index))
          (cadar index)
         else
          (begin
            (display-list "invalid choice, try again (" choice-range ") > ")
            (select-loop)))))

    (select-loop)))
