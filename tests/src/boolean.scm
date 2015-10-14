(write (#t 'true 'false))
(newline)

(write (if (< 1 2) #t #f))
(newline)

(write (if (> 3 2)
           (if (< 2 1)
             #t
             #f)
           #f))
(newline)
