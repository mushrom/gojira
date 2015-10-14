(write "Hello, world")
(newline)

(write (eq? (string->symbol "asdf") 'asdf))
(newline)

(write (string-contains "some string" "str"))
(newline)

(write (string-contains "some string" "saywut"))
(newline)

(write (string-append "testing " "this"))
(newline)

(write (symbol->string 'testing))
(newline)

; test string escapes
(write "testing\"this \\")
(newline)

(write "\\\\\"\"\\")
(newline)

(write "\a\b\c\\d")
(newline)
