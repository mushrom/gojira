#!/usr/bin/env gojira
; This is a modified version of the multitasking example from wikipedia,
; at https://en.wikipedia.org/wiki/Call-with-current-continuation

;; Cooperative multitasking using call-with-current-continuation
;; in 25 lines of scheme

;; The list of threads waiting to run. This is a list of one
;; argument non-returning functions (continuations, mostly)
;; A continuation is a non-returning function, just like (exit),
;; in that it never gives up control to whoever called it.

(define readyList '())

;; A non-returning function. If there is any other thread
;; waiting to be run, it causes the next thread to run if there
;; is any left to run, otherwise it calls the original exit
;; which exits the whole environment.
;(define (exit) (print "lol"))
(define (exit-thread)
  (unless (null? readyList)
    ;; There is another thread waiting to be run.
    ;; So we run it.
    (let ((cont (car readyList)))
      (set! readyList (cdr readyList))
      ;; Since the readyList is only non-returning
      ;; functions, this will not return.
      (cont '()))))

;; Takes a one argument function with a given
;; argument and forks it off.  The forked function's new
;; thread will exit if/when the function ever exits.
(define (fork fn arg)
  (set! readyList
    (append readyList
            ;; This function added to the 
            ;; readyList is non-returning,
            ;; since exit is non returning.
            (cons
              (lambda (x)
                (fn arg)
                (exit)) '()))))

;; Gives up control for the next thread waiting to be run.
;; Although it will eventually return, it gives up control
;; and will only regain it when the continuation is called.
(define (yield)
  (call-with-current-continuation
    ;; Capture the continuation representing THIS call to yield
    (lambda (thisCont)
      ;; Stick it on the ready list
      (set! readyList
        (append readyList
                (cons thisCont '())))
      ;; Get the next thread, and start it running.
      (let ((cont (car readyList)))
        (set! readyList (cdr readyList))
        ;; Run it.
        (cont '())))))

;; Start evaluating the thread list by 'exiting' the main thread
(define (start-threading)
  (exit-thread))

(define (inc-thread sum)
  (if (< sum 100000)
    (begin
      (print sum)
      (yield)
      (inc-thread (+ sum 1)))
   else
    (exit-thread)))

(define (printer-thread thing)
  (print thing)
  (yield)
  (printer-thread thing))

(fork inc-thread 0)
(fork printer-thread :testing)
(fork printer-thread '(this is a thing))
(start-threading)
