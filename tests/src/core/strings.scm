;; => "Hello, world"
(write "Hello, world")
(newline)

;; => #t
(write (eq? (string->symbol "asdf") 'asdf))
(newline)

;; => 5
(write (string-contains "some string" "str"))
(newline)

;; => #f
(write (string-contains "some string" "saywut"))
(newline)

;; => "testing this"
(write (string-append "testing " "this"))
(newline)

;; => "testing"
(write (symbol->string 'testing))
(newline)

; test string escapes
;; => "testing\"this \\"
(write "testing\"this \\")
(newline)

;; => "\\\\\"\"\\"
(write "\\\\\"\"\\")
(newline)

;; => "abc\\d"
(write "\a\b\c\\d")
(newline)
