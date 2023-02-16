#lang racket
(require "simpleParser.rkt")

(define keyword-math-operators '(+ - * / %))
(define keyword-bool-operators '(&& || !))
(define keyword-comparators    '(== != < > <= >=))
(define keyword-control        '(var = return if while))

(define NULL 'null)
(define RETURN 'return)

; === Main ===
; Interpreter entry point. Reads a file as a program and interprets it, returning the return value of the program
(provide interpret)
(define interpret
  (lambda (filename)
    (interpreter-helper (parser filename) '(() ()))))

(define interpreter-helper
  (lambda (statementlist state)
    (if (check-for-binding RETURN state)
        (get-binding-value RETURN state)
        (interpreter-helper (cdr statementlist) (m-state (car statementlist) state)))))
  

; === State helper functions ===
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

; Check whether a name is bound
(define check-for-binding
  (lambda (name state)
    (contains? name (get-state-names state))))

; Find the value for a bound name
(define get-binding-value
  (lambda (name state)
    (cond
      ((null? state)                            (error "Name not bound"))
      ((eq? (car (get-state-names state)) name) (car (get-statement-values state)))
      (else                                     (get-binding-value name (make-state (cdr (get-state-names state)) (cdr (get-state-values state))))))))

; Add a name-value pair binding to the state, or replce the value if name is already bound
(define add-binding
  (lambda (name value state)
    (add-binding-cps name value state (lambda (v) v))))

(define add-binding-cps
  (lambda (name value state return)
    (cond
      ((null? (get-state-names state))          (return (make-state (list name) (list value))))
      ((eq? (car (get-state-names state)) name) (return (make-state (get-state-names state) (cons value (cdr (get-state-values state))))))
      (else                                     (add-binding-cps name value (make-state (cdr (get-state-names state)) (cdr (get-state-values state)))
                                                             (lambda (v) (return (make-state
                                                                                  (cons (car (get-state-names  state)) (get-state-names  v))
                                                                                  (cons (car (get-state-values state)) (get-state-values v))))))))))

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

; === State handler ===
; Modify the state by a statement
(define m-state
  (lambda (statement state)
    (cond
      ((null? statement)                                                 state)
      ((contains? (get-statement-type statement) keyword-math-operators) (m-state-math-operators statement state))
      ((contains? (get-statement-type statement) keyword-bool-operators) (m-state-bool-operators statement state))
      ((contains? (get-statement-type statement) keyword-comparators)    (m-state-comparators    statement state))
      ((contains? (get-statement-type statement) keyword-control)        (m-state-control        statement state))
      (else                                                              (error "Unknown keyword")))))

; === Numerical operator handlers ===
(define m-state-math-operators
  (lambda (statement state)
    state))

; === Boolean operator handlers ===
(define m-state-bool-operators
  (lambda (statement state)
    state))

; === Comparison operator handlers ===
(define m-state-comparators
  (lambda (statement state)
    state))

; === Control flow statement handlers ===
(define m-state-control
  (lambda (statement state)
    (cond
      ((eq? (get-statement-type statement) 'var)    (m-state-var    statement state))
      ((eq? (get-statement-type statement) 'return) (m-state-return statement state))
    )))

(define get-var-name
  (lambda (statement)
    (cadr statement)))

(define get-var-value
  (lambda (statement state)
    (if (null? (cddr statement))
        NULL
        (m-number (caddr statement) state))))

; var statement handler
(define m-state-var
  (lambda (statement state)
    (if (check-for-binding (get-var-name statement) state)
        (error "variable already declared")
        (add-binding (get-var-name statement) (get-var-value statement state) state))))

(define get-return-value
  (lambda (statement state)
    (m-number (cadr statement) state)))

; return statement handler
(define m-state-return
  (lambda (statement state)
    (add-binding RETURN (get-return-value statement state))))

; === Numerical expresion evaluator
; TODO
(define m-number
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      (else 0))))

; === Boolearn expression evaluator
; TODO
(define m-bool
  (lambda (expression state)
    #f))