(display "Hello, world")
(newline)

(display (eq? (string->symbol "asdf") 'asdf))
(newline)

(display (string-contains "some string" "str"))
(newline)

(display (string-contains "some string" "saywut"))
(newline)

(display (string-append "testing " "this"))
(newline)

(display (symbol->string 'testing))
(newline)

; test string escapes
(display "testing\"this \\")
(newline)

(display "\\\\\"\"\\")
(newline)

(display "\a\b\c\\d")
(newline)
