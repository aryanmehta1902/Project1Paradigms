#lang racket

(define interactive?
   (let [(args (current-command-line-arguments))]
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
    
    ; Multiplication operator
    [(and (not (null? chars)) (char=? (car chars) #\*))
     (let* ([first-eval (evaluate-expression (cdr chars) history)]
            [first-val (car first-eval)]
            [rest-chars (cdr first-eval)])
       (if first-val
           (let* ([second-eval (evaluate-expression rest-chars history)]
                  [second-val (car second-eval)]
                  [remaining (cdr second-eval)])
             (if second-val
                 (cons (* first-val second-val) remaining)
                 (cons #f rest-chars)))
           (cons #f (cdr chars))))]
    
    ; Division operator (integer division)
    [(and (not (null? chars)) (char=? (car chars) #\/))
     (let* ([first-eval (evaluate-expression (cdr chars) history)]
            [first-val (car first-eval)]
            [rest-chars (cdr first-eval)])
       (if first-val
           (let* ([second-eval (evaluate-expression rest-chars history)]
                  [second-val (car second-eval)]
                  [remaining (cdr second-eval)])
             (if second-val
                 (if (= second-val 0)
                     (cons #f rest-chars) ; Division by zero error
                     (cons (quotient (inexact->exact (floor first-val)) 
                                    (inexact->exact (floor second-val))) 
                           remaining))
                 (cons #f rest-chars)))
           (cons #f (cdr chars))))]
    
    ; Negation operator
    [(and (not (null? chars)) (char=? (car chars) #\-))
     (let* ([eval-result (evaluate-expression (cdr chars) history)]
            [val (car eval-result)]
            [remaining (cdr eval-result)])
       (if val
           (cons (- val) remaining)
           (cons #f (cdr chars))))]
    
    ; History lookup operator
    [(and (not (null? chars)) (char=? (car chars) #\$))
     (let* ([id-chars (get-number (cdr chars))]
            [id-str (list->string (car id-chars))]
            [remaining (cdr id-chars)])
       (if (string=? id-str "")
           (cons #f chars)
           (let ([id (string->number id-str)])
             (if (and id (> id 0) (<= id (length history)))
                 (cons (list-ref (reverse history) (- id 1)) remaining)
                 (cons #f chars)))))]
    
    ; Number literal
    [(and (not (null? chars)) (or (char-numeric? (car chars)) (char=? (car chars) #\.)))
     (let* ([num-chars (get-number chars)]
            [num-str (list->string (car num-chars))]
            [remaining (cdr num-chars)])
       (cons (string->number num-str) remaining))]
    
    ; No match - error
    [else (cons #f chars)]))

(define (get-number chars)
  (define (collect-digits acc remaining)
    (cond
      [(null? remaining) (cons (reverse acc) '())]
      [(or (char-numeric? (car remaining)) (char=? (car remaining) #\.))
       (collect-digits (cons (car remaining) acc) (cdr remaining))]
      [else (cons (reverse acc) remaining)]))
  
  (collect-digits '() chars))

; Main evaluation loop
(define (evaluation-loop history)
  (display-prompt)
  (let ([input (read-line)])
    (cond
      ; Handle EOF
      [(eof-object? input) (void)]
      
      ; Handle quit command
      [(string=? input "quit") (void)]
      
      ; Process expression
      [else
       (let* ([chars (string->list input)]
              [result (evaluate-expression chars history)])
         (cond
           ; Valid expression with no remaining text
           [(and (car result) (null? (cdr result)))
            (let ([value (car result)])
              (let ([float-val (real->double-flonum value)]
                    [history-id (+ 1 (length history))])
                (displayln (string-append (number->string history-id) ": " (number->string float-val)))
                (evaluation-loop (cons value history))))]
           
           ; Valid expression but with extra text
           [(and (car result) (not (null? (cdr result))))
            (handle-error "Invalid Expression: Extra text after valid expression")
            (evaluation-loop history)]
           
           ; Invalid expression
           [else
            (handle-error "Invalid Expression")
            (evaluation-loop history)]))])))

; Start the calculator
(evaluation-loop '())
