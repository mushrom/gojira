(import! 'ansi-term)

(define :mut *docs-list* '())
(define :mut doc-title-color '(:magenta))

; see bottom of the file for doc definitions on these syntax rules
(define-syntax docs
  (syntax-rules ()
    ((_ sym)
     (print-docpage (get-docpage 'sym)))))

(define-syntax document
  (syntax-rules ()
    ((_ func-def docinfo ...)
     (add-docpage! (list (car '(func-def)) (hashmap docinfo ...))))))

(define-syntax define/docs
  (syntax-rules ()
    ((_ func-def (docinfo ...) body ...)
     (begin
       (document func-def docinfo ...)
       (define func-def body ...)))))

(define (add-docpage! doc)
  (define *docs-list* (cons doc *docs-list*)))

(define (get-docpage doc)
  (filter (lambda (page)
            (eq? (caar page) doc))
          *docs-list*))

(define (print-doc-field disp-func title info)
  (if (not (eq? info #f))
    (begin
      (display "  ")
      (print-color doc-title-color title)
      (if (list? info)
        (for-each (lambda (thing)
                    (display "    ")
                    (disp-func thing)
                    (newline))
                  info)
        (begin
          (display "    ")
          (disp-func info)
          (newline))))
    '())
  '())

(define (list-max nums)
  (if (null? nums)
    0
    (let ((max-val (list-max (cdr nums))))
      (if (> max-val (car nums))
        max-val
        (car nums)))))

(define (max :rest nums)
  (list-max nums))

(define (print-doc-parameters title info)

  (define (make-aligned-printer xs)
    (let ((alignment
            (list-max
              (map (lambda (x)
                     (string-length (symbol->string (car x))))
                   xs))))

      (lambda (sym)
        (display sym)
        (for-each (lambda (x) (display #\space))
                  (iota (+ (- alignment
                              (string-length
                                (symbol->string sym)))
                           1))))))

  (if (not (eq? info #f))
    (begin
      (display "  ")
      (print-color doc-title-color title)

      (define param-display (make-aligned-printer info))

      (for thing in info
           (display "    ")
           (param-display (car thing))
           (display-list ": " (cadr thing) #\newline)
           (if (not (null? (cddr thing)))
             (let ((sub-arg-display (make-aligned-printer (caddr thing))))
               (for sub-arg in (caddr thing)
                    (display "    ")
                    (param-display (string->symbol " "))
                    (display "> ")
                    (sub-arg-display (car sub-arg))
                    (display-list ": " (cadr sub-arg) #\newline)))
               '()))
    '())
  '()))

(define (print-docpage doc)
  (if (not (null? doc))
    (let* ((func-def  (caar doc))
           (func-name (car func-def))
           (docmap    (cadar doc))
           (usage     (if (eq? [docmap :usage] #f)
                        (list func-def)
                       else
                        [docmap :usage])))

      (print-doc-field display "Usage:" usage)
      (print-doc-field display "Description:" [docmap :description])
      (print-doc-parameters    "Parameters:" [docmap :parameters])
      (print-doc-field display "Returns:" [docmap :returns])
      (print-doc-field display "See Also:" [docmap :see-also])
      (print-doc-field display "Example:" [docmap :example])
      (print-doc-field display "Conformance:" [docmap :conformance]))

   else
    (print "No documentation, sorry")))

(document (docs name)
    :description '("Prints short documentation corresponding to the given name")
    :parameters  '((name "The symbol name of the function for which to show documentation"))
    :returns     '("A null list")
    :see-also    '(document define/docs)
    :example     '((docs docs))
    :conformance '(non-standard))

(document (document func-def docinfo ...)
    :description '("Generates a documentation page for the given function")
    :parameters  '((func-def "The function definition for the documentee")
                   (docinfo  "Various information about the function"
                              ((:description "A brief overview of what the function does")
                               (:parameters  "Description of what the function's parameters expect")
                               (:usage       "Optional list of the possible ways the function may be called")
                               (:returns     "A description of the values the function returns")
                               (:see-also    "A short list of other functions which might be of interest")
                               (:example     "An example usage of the function")
                               (:conformance "A list of standards the function conforms to, eg. r6rs, srfi1"))))
    :conformance '(non-standard)
    :returns     '("A null list")
    :see-also    '(define/docs docs)
    :example     '("(document (some-function foo bar)"
                   "   :description '(\"A minimal example doc page\")"
                   "   :parameters  '((foo \"A symbol\""
                   "                    ((sub-arg"
                   "                      \"An example of a sub-argument\")"
                   "                     (blarg"
                   "                      \"Sub-arguments specify possible values a parameter might have\")))"
                   "                  (bar \"A string\"))"
                   "   :returns '(\"The integer 42\"))"))

(document (define/docs func-def (docinfo ...) body ...)
    :description '("A macro wrapper around 'document' and 'define' to neatly group"
                   "function definitions and documentation definitions together.")

    :parameters  '((func-def "The function definition")
                   (docinfo  "Function information to be passed to 'document'")
                   (body     "The body of the function"))
    :conformance '(non-standard)
    :see-also    '(document docs))
