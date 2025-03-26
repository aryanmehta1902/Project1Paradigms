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

(define (evaluate-expression chars history)
  (cond
    ; Skip whitespace
    [(and (not (null? chars)) (char-whitespace? (car chars)))
     (evaluate-expression (cdr chars) history)]
    
    ; Addition operator
    [(and (not (null? chars)) (char=? (car chars) #\+))
     (let* ([first-eval (evaluate-expression (cdr chars) history)]
            [first-val (car first-eval)]
            [rest-chars (cdr first-eval)])
       (if first-val
           (let* ([second-eval (evaluate-expression rest-chars history)]
                  [second-val (car second-eval)]
                  [remaining (cdr second-eval)])
             (if second-val
                 (cons (+ first-val second-val) remaining)
                 (cons #f rest-chars)))
           (cons #f (cdr chars))))]
    
    ; Fallback error clause for unrecognized expressions
    [else (cons #f chars)]))
