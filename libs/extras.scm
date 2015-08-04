(define-syntax while
  (syntax-rules ()
    ((_ con body ...)

     (begin
       (define loopthing
         (lambda ()
           (if con
             (begin
               (begin body ...)
               (loopthing))
             #f)))
       (loopthing)))))

(define-syntax match-value
  (syntax-rules ()
    ((_ x (value body ...) more ...)
     (if (eq? x 'value)
       (begin body ...)
     (if (symbol? 'value)
       (begin
         (intern-set 'value x)
         body ...)
       (match-value x more ...))))

    ((_ x (value body ...))
     (if (eq? x 'value)
       (begin body ...) 

     (if (symbol? 'value)
       (begin
         (intern-set 'value x)
         body ...)
       #f)))))

(define-syntax match
  (syntax-rules ()
    ((_ sym patterns ...)
     (lambda (sym) (match-value sym patterns ...)))))
