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

; Check whether an operand is a literal or a variable
(define is-literal?
  (lambda (statement)
    (cond
      ((number? statement)       #t)
      ((equal? 'true statement)  #t)
      ((equal? 'false statement) #t)
      (else                      #f))))

; Check whether a literal is a boolean
(define bool?
  (lambda (expression)
    (cond
      ((equal? expression 'true)  #t)
      ((equal? expression 'false) #t)
      (else                       #f))))

; Check whether a name is bound
(define check-for-binding
  (lambda (name state)
    (contains? name (get-state-names state))))

; Find the value for a bound name
(define get-binding-value
  (lambda (name state)
    (cond
      ((null? state)                            (error "Name not bound"))
      ((eq? (car (get-state-names state)) name) (car (get-state-values state)))
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

(define m-state-math-operators
  (lambda (statement state)
    state))

(define m-state-bool-operators
  (lambda (statement state)
    state))

(define m-state-comparators
  (lambda (statement state)
    state))

; === Control flow state handlers ===
(define m-state-control
  (lambda (statement state)
    (cond
      ((eq? (get-statement-type statement) 'var)    (m-state-var    statement state))
      ((eq? (get-statement-type statement) 'return) (m-state-return statement state))
      ((eq? (get-statement-type statement) '=)      (m-state-assign statement state)))))

(define get-var-name
  (lambda (statement)
    (cadr statement)))

(define get-var-value
  (lambda (statement state)
    (cond
      ((null?     (cddr statement)) NULL)
      ((number?   (cddr statement)) (m-number (caddr statement)))
      ((bool?     (cddr statement)) (m-bool (caddr statement))))))

; var statement handler
(define m-state-var
  (lambda (statement state)
    (if (check-for-binding (get-var-name statement) state)
        (error "variable already declared")
        (add-binding (get-var-name statement) (get-var-value statement state) state))))

(define get-return-value
  (lambda (statement state)
    (m-value (cadr statement) state)))

; return statement handler
(define m-state-return
  (lambda (statement state)
    (add-binding RETURN (get-return-value statement state) state)))

; assign statement handler
(define m-state-assign
  (lambda (statement state)
    (if (check-for-binding (get-assign-name statement) state)
        (add-binding (get-assign-name statement) (get-assign-value statement) state)
        (error "Cannot assign to a variable that has not been declared"))))

(define get-assign-name
  (lambda (statement)
    (cadr statement)))

(define get-assign-value
  (lambda (statement)
    (caddr statement)))

; === Values expression evaluator
; TODO (maybe not i might have actually done it)
(define m-value
  (lambda (expression state)
    (cond
      ((number? expression)                                         (m-number expression state))
      ((contains? (get-operator expression) keyword-math-operators) (m-number expression state))
      (else                                                         (m-bool   expression state)))))

; Get the operator from a expression represented by a list
(define get-operator
  (lambda (expression)
    (car expression)))

; Get the first operand from a expression represented by a list
(define get-first-operand
  (lambda (expression)
    (cadr expression)))

; Get the second operand from a expression represented by a list
(define get-second-operand
  (lambda (expression)
    (caddr expression)))

; Checks to see if there is a second operand in an expression represented by a list
(define second-operand-exists?
  (lambda (expression)
    (if (null? (cddr expression))
        #f
        #t)))

(define is-bool-expression?
  (lambda (expression)
    (cond
      ((eq? expression 'true)                                       #t)
      ((eq? expression 'false)                                      #t)
      ((contains? (get-operator expression) keyword-bool-operators) #t)
      ((contains? (get-operator expression) keyword-comparators)    #t)
      (else                                                         #f))))

; === Numerical expresion evaluator ===
(define m-number
  (lambda (expression state)
    (cond
      ((number? expression)                                         expression)
      ((contains? (get-operator expression) keyword-math-operators) (m-number-math-operators expression state))
      ((is-bool-expression? expression)                             (if (m-bool expression state) 1 0))  ; Cast bool to number
      (else                                                         (error "This isn't a numerical expression")))))

; === Numerical expresion evaluators ===
(define m-number-math-operators
  (lambda (expression state)
    (cond
      ((eq? (get-operator expression) '+) (m-number-addition       expression state))
      ((eq? (get-operator expression) '-) (m-number-subtraction    expression state))
      ((eq? (get-operator expression) '*) (m-number-multiplication expression state))
      ((eq? (get-operator expression) '/) (m-number-division       expression state))
      ((eq? (get-operator expression) '%) (m-number-modulus        expression state)))))

; addition expresion evaluator
(define m-number-addition
  (lambda (expression state)
    (+
     (m-number (get-first-operand expression)  state)
     (m-number (get-second-operand expression) state))))

; subtraction expresion evaluator
(define m-number-subtraction
  (lambda (expression state)
    (if (second-operand-exists? expression)
        (-
         (m-number (get-first-operand expression) state)
         (m-number (get-second-operand expression) state))
        (- 0 (m-number (get-first-operand expression) state)))))

; multiplication expresion evaluator
(define m-number-multiplication
  (lambda (expression state)
    (*
     (m-number (get-first-operand expression)  state)
     (m-number (get-second-operand expression) state))))

; division expresion evaluator
(define m-number-division
  (lambda (expression state)
    (/
     (m-number (get-first-operand expression)  state)
     (m-number (get-second-operand expression) state))))

; modulus expresion evaluator
(define m-number-modulus
  (lambda (expression state)
    (modulo
     (m-number (get-first-operand expression)  state)
     (m-number (get-second-operand expression) state))))


; === Boolean expression evaluator ===
(define m-bool
  (lambda (expression state)
    (cond
      ((equal? expression 'true)                                    #t)
      ((equal? expression 'false)                                   #f)
      ((contains? (get-operator expression) keyword-bool-operators) (m-state-bool-operators   expression state))
      ((contains? (get-operator expression) keyword-comparators)    (m-state-comparators      expression state))
      (else                                                         (error "This isn't a boolean expression")))))

; === Comparison operator expression evaluator ===
(define m-bool-comparators
  (lambda (expression state)
    (cond
      ((eq? (get-operator expression) '==) (m-bool-equals              expression state))
      ((eq? (get-operator expression) '!=) (m-bool-not-equals          expression state))
      ((eq? (get-operator expression) '<)  (m-bool-less-than           expression state))
      ((eq? (get-operator expression) '>)  (m-bool-greater-than        expression state))
      ((eq? (get-operator expression) '<=) (m-bool-less-than-equals    expression state))
      ((eq? (get-operator expression) '>=) (m-bool-greater-than-equals expression state)))))

; equals expression evaluator
(define m-bool-equals
  (lambda (expression state)
    (equal?
     (m-bool (get-first-operand expression)  state)
     (m-bool (get-second-operand expression) state))))

; not equals expression evaluator
(define m-bool-not-equals
  (lambda (expression state)
    (m-bool (get-first-operand expression)  state)
    (m-bool (get-second-operand expression) state)))

; less than expression evaluator
(define m-bool-less-than
  (lambda (expression state)
    (<
     (m-bool (get-first-operand expression)  state)
     (m-bool (get-second-operand expression) state))))

; greater than expression evaluator
(define m-bool-greater-than
  (lambda (expression state)
    (>
     (m-bool (get-first-operand expression)  state)
     (m-bool (get-second-operand expression) state))))

; less than or equal to expression evaluator
(define m-bool-less-than-equals
  (lambda (expression state)
    (<=
     (m-bool (get-first-operand expression)  state)
     (m-bool (get-second-operand expression) state))))
       

; greater than or equal to expression evaluator
(define m-bool-greater-than-equals
  (lambda (expression state)
    (>=
     (m-bool (get-first-operand expression)  state)
     (m-bool (get-second-operand expression) state))))

; === Boolean operator expression handlers ===
(define m-bool-bool-operators
  (lambda (expression state)
    (cond
      ((eq? (get-operator expression) '!)  (m-bool-not expression state))
      ((eq? (get-operator expression) '&&) (m-bool-and expression state))
      ((eq? (get-operator expression) '||) (m-bool-or  expression state)))))

; not expression handler
(define m-bool-not
  (lambda (expression state)
    (not (m-bool (get-first-operand expression) state))))

; and expression handler
(define m-bool-and
  (lambda (expression state)
    (and
     (m-bool (get-first-operand expression)  state)
     (m-bool (get-second-operand expression) state))))

; or expression handler
(define m-bool-or
  (lambda (expression state)
    (or
     (m-bool (get-first-operand expression)  state)
     (m-bool (get-second-operand expression) state))))