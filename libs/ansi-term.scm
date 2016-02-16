(define (u8-bytevector :rest bytes)
  (u8-list->bytevector bytes))

(define ansi-term
  (let ((fg-color (lambda (val)
                    (u8-bytevector #x1b #x5b #x33 val #x6d)))
        (bg-color (lambda (val)
                    (u8-bytevector #x1b #x5b #x34 val #x6d))))
    (hashmap
      :foreground
        (hashmap
          :black   (fg-color #x30)
          :red     (fg-color #x31)
          :green   (fg-color #x32)
          :yellow  (fg-color #x33)
          :blue    (fg-color #x34)
          :magenta (fg-color #x35)
          :cyan    (fg-color #x36)
          :white   (fg-color #x37))

      :background
        (hashmap
          :black   (bg-color #x30)
          :red     (bg-color #x31)
          :green   (bg-color #x32)
          :yellow  (bg-color #x33)
          :blue    (bg-color #x34)
          :magenta (bg-color #x35)
          :cyan    (bg-color #x36)
          :white   (bg-color #x37))

      :control
        (hashmap
          :reset-color (u8-bytevector #x1b #x5b #x30 #x30 #x6d)))))

(define (ansi-term-test)
  (map display (list [[ansi-term :foreground] :red]          "Testing "
                     [[ansi-term :foreground] :green]        "this "
                     [[ansi-term :foreground] :blue]         "thing "
                     [[ansi-term :foreground] :yellow]       "here "
                     [[ansi-term :control]    :reset-color]  "man"
                     #\newline)))

(define (display-color colorspec value)
  (with colorspec as (fg bg)
    (if (not (null? fg))
      (display [[ansi-term :foreground] fg])
      '())

    (if (not (null? bg))
      (display [[ansi-term :background] bg])
      '())

    (display value)

    (display [[ansi-term :control] :reset-color])))

(define (print-color colorspec value)
  (display-color colorspec value)
  (newline))
