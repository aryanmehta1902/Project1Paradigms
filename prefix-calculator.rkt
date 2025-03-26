#lang racket

(define interactive?
  (let ([args (current-command-line-arguments)])
    (cond
      [(= (vector-length args) 0) #t]
      [(string=? (vector-ref args 0) "-b") #f]
      [(string=? (vector-ref args 0) "--batch") #f]
      [else #t])))

(define (display-prompt)
  (when interactive?
    (display "Enter an expression: ")
    (flush-output)))

(define (handle-error message)
  (displayln (string-append "Error: " message))
  #f)
