#lang racket
(require "simpleParser.rkt")

(define keyword-math-operators '(+ - * / %))
(define keyword-bool-operators '(&& || !))
(define keyword-comparators    '(== != < > <= >=))
(define keyword-control        '(var = return if while))

(define NULL 'null)


(define interpret
  (lambda (filename)
    (parser filename)))

(define contains?
  (lambda (element list)
    (cond
      ((null? list)             #f)
      ((eq? (car list) element) #t)
      (else                     (contains? element (cdr list))))))

(define get-state-names
  (lambda (state)
    (car state)))

(define get-state-values
  (lambda (state)
    (cadr state)))

(define make-state
  (lambda (names values)
    (list names values)))

(define add-binding-cps
  (lambda (name value state return)
    (cond
      ((null? (get-state-names state))          (return (make-state (list name) (list value))))
      ((eq? (car (get-state-names state)) name) (return (make-state (get-state-names state) (cons value (cdr (get-state-values state))))))
      (else                                     (add-binding-cps name value (make-state (cdr (get-state-names state)) (cdr (get-state-values state)))
                                                             (lambda (v) (return (make-state
                                                                                  (cons (car (get-state-names  state)) (get-state-names  v))
                                                                                  (cons (car (get-state-values state)) (get-state-values v))))))))))

(define add-binding
  (lambda (name value state)
    (add-binding-cps name value state (lambda (v) v))))
    

; Get the keyword the defines the statment type from a statment represented by a list
(define get-statement-type
  (lambda (statement)
    (car statement)))


; Get the operator from a expression represented by a list
(define get-operator
  (lambda (statement)
    (car statement)))

; Get the first operand from a expression represented by a list
(define get-first-operand
  (lambda (statement)
    (cadr statement)))

; Get the second operand from a expression represented by a list
(define get-second-operand
  (lambda (statement)
    (caddr statement)))

(define m-state
  (lambda (statement state)
    (cond
      ((null? statement)                                          state)
      ((contains? (get-operator statement) keyword-math-operators) (m-state-math-operators statement state))
      ((contains? (get-operator statement) keyword-bool-operators) (m-state-bool-operators statement state))
      ((contains? (get-operator statement) keyword-comparators)    (m-state-comparators    statement state))
      ((contains? (get-operator statement) keyword-control)        (m-state-control        statement state))
      (else (error "Unknown keyword")))))

#|
(define m-state-math-operators
  (lambda (statement state)
    (cond
      ((eq? (get-operator statment) '+) (
|#

(define check-for-binding
  (lambda (name state)
    (contains? (car state))))

(define m-state-math-operators
  (lambda (statement state)
    state))

(define m-state-bool-operators
  (lambda (statement state)
    state))

(define m-state-comparators
  (lambda (statement state)
    state))

(define m-state-control
  (lambda (statement state)
    state))

(define get-var-name
  (lambda (statement)
    (cadr statement)))

(define get-var-value
  (lambda (statement)
    (if (null? (cdr statement))
        NULL
        (m-number (cadr statement)))))

(define m-state-var
  (lambda (statement state)
    (if (check-for-binding (get-var-name statement))
        (error "variable already declared")
        (add-binding (get-var-name statement) (get-var-value statement) state))))

#|
(define m-state-control
  (lambda (statement state)
    (cond
      ((eq? (get-statment-type statement) 'var) 
    ))
|#

; TODO
(define m-number
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      (else 0))))