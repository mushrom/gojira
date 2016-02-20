#!/usr/bin/env gojira

; numbers ordered from 0-9
(define nums
  '((" _ " "| |" "|_|")
    ("   " "  |" "  |")
    (" _ " " _|" "|_ ")
    (" _ " " _|" " _|")
    ("   " "|_|" "  |")
    (" _ " "|_ " " _|")
    (" _ " "|_ " "|_|")
    (" _ " "  |" "  |")
    (" _ " "|_|" "|_|")
    (" _ " "|_|" " _|")))

(define (make-row-printer n fn)
  (let* ((current   (modulo n 10))
         (curstring (fn (list-ref nums current)))
         (next      (floor (/ n 10)))
         (nextfunc  (if (< n 10)
                      newline
                      (make-row-printer next fn))))

    (lambda ()
      (nextfunc)
      (display curstring))))

(define (make-calc-printer number)
  (let ((funcs (map (lambda (fn)
                      (make-row-printer number fn))
                    (list car cadr caddr))))

    (lambda ()
      (map (lambda (f) (f)) funcs)
      (newline))))

(define foo
  (map make-calc-printer
       '(123
         1337
         #x1234
         1234567890)))

(map (lambda (f) (f)) foo)
