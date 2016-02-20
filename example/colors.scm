#!/usr/bin/env gojira

(import! 'ansi-term)
(import! 'prompt)

(define colors '(:black :red :green :yellow :blue :magenta :cyan :white))

(define (prompt-colorspec)
  (list (select-item "What foreground color?" colors)
        (select-item "What background color?" colors)))

(define (test-colors color)
  (print-color color "Testing colorful text output")
  (print-color color "This is some very colorful text")
  (print-color color "Very nice and colorful."))

(test-colors (prompt-colorspec))
