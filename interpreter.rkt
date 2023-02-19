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

; Checks to see if there is a second operand in an expression represented by a list
(define second-operand-exists?
  (lambda (statement)
    (if (null? (cddr statement))
        #f
        #t)))

; Evaluate the first operand from an expression represented by a list
(define evaluate-first-operand
  (lambda (statement state)
    (if (is-literal? (get-first-operand statement))
               (get-first-operand statement)
               (m-state (get-first-operand) state))))

; Evaluate the second operand from an expression represented by a list
(define evaluate-second-operand
  (lambda (statement state)
    (if (is-literal? (get-second-operand statement))
               (get-second-operand statement)
               (m-state (get-second-operand) state))))

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

; === Numerical operator state handlers ===
(define m-state-math-operators
  (lambda (statement state)
    (cond
      ((eq? (get-statement-type statement) '+) (m-state-addition       statement state))
      ((eq? (get-statement-type statement) '-) (m-state-subtraction    statement state))
      ((eq? (get-statement-type statement) '*) (m-state-multiplication statement state))
      ((eq? (get-statement-type statement) '/) (m-state-division       statement state))
      ((eq? (get-statement-type statement) '%) (m-state-modulus        statement state)))))

; addition statement handler
(define m-state-addition
  (lambda (statement state)
    (+
     (evaluate-first-operand  statement state)
     (evaluate-second-operand statement state))))

; subtraction statement handler
(define m-state-subtraction
  (lambda (statement state)
    (if (second-operand-exists? statement)
        (-
         (evaluate-first-operand     statement state)
         (evaluate-second-operand    statement state))
        (- 0 (evaluate-first-operand statement state)))))

; multiplication statement handler
(define m-state-multiplication
  (lambda (statement state)
    (*
     (evaluate-first-operand  statement state)
     (evaluate-second-operand statement state))))

; division statement handler
(define m-state-division
  (lambda (statement state)
    (/
     (evaluate-first-operand  statement state)
     (evaluate-second-operand statement state))))

; modulus statement handler
(define m-state-modulus
  (lambda (statement state)
    (modulo
     (evaluate-first-operand  statement state)
     (evaluate-second-operand statement state))))

; === Boolean operator state handlers ===
(define m-state-bool-operators
  (lambda (statement state)
    (cond
      ((eq? (get-statement-type statement) '!) (m-state-not  statement state))
      ((eq? (get-statement-type statement) '&&) (m-state-and statement state))
      ((eq? (get-statement-type statement) '||)  (m-state-or statement state)))))

; not statement handler
(define m-state-not
  (lambda (statement state)
    (if (equal?
         (evaluate-first-operand  statement state)
         'true)
        false
        true)))

; and statement handler
(define m-state-and
  (lambda (statement state)
    (if (and
         (evaluate-first-operand  statement state)
         (evaluate-second-operand statement state))
        false
        true)))

; or statement handler
(define m-state-or
  (lambda (statement state)
    (if (or
         (evaluate-first-operand  statement state)
         (evaluate-second-operand statement state))
        false
        true)))

; === Comparison operator state handlers ===
(define m-state-comparators
  (lambda (statement state)
    (cond
      ((eq? (get-statement-type statement) '==) (m-state-equals              statement state))
      ((eq? (get-statement-type statement) '!=) (m-state-not-equals          statement state))
      ((eq? (get-statement-type statement) '<)  (m-state-less-than           statement state))
      ((eq? (get-statement-type statement) '>)  (m-state-greater-than        statement state))
      ((eq? (get-statement-type statement) '<=) (m-state-less-than-equals    statement state))
      ((eq? (get-statement-type statement) '>=) (m-state-greater-than-equals statement state)))))

; equals statement handler
(define m-state-equals
  (lambda (statement state)
    (if (equal?
         (evaluate-first-operand  statement state)
         (evaluate-second-operand statement state))
        true
        false)))

; not equals statement handler
(define m-state-not-equals
  (lambda (statement state)
    (if (equal?
         (evaluate-first-operand  statement state)
         (evaluate-second-operand statement state))
        false
        true)))

; less than statement handler
(define m-state-less-than
  (lambda (statement state)
    (if (<
         (evaluate-first-operand  statement state)
         (evaluate-second-operand statement state))
        false
        true)))

; greater than statement handler
(define m-state-greater-than
  (lambda (statement state)
    (if (>
         (evaluate-first-operand  statement state)
         (evaluate-second-operand statement state))
        false
        true)))

; less than or equal to statement handler
(define m-state-less-than-equals
  (lambda (statement state)
    (if (or
         (equal?
          (evaluate-first-operand  statement state)
          (evaluate-second-operand statement state))
         (<
          (evaluate-first-operand  statement state)
          (evaluate-second-operand statement state)))
        true
        false)))
       

; greater than or equal to statement handler
(define m-state-greater-than-equals
  (lambda (statement state)
    (if (or
         (equal?
          (evaluate-first-operand  statement state)
          (evaluate-second-operand statement state))
         (>
          (evaluate-first-operand  statement state)
          (evaluate-second-operand statement state)))
        true
        false)))

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

; === Numerical expresion evaluator
(define m-number
  (lambda (expression state)
    (cond
      ((number? expression)                                         expression)
      ((equal? expression 'true)                                    1)
      ((equal? expression 'false)                                   0)
      ((contains? (get-operator expression) keyword-math-operators) (m-state-math-operators expression state))
      (else                                                         (error "This isn't a numerical expression")))))

; === Boolearn expression evaluator
(define m-bool
  (lambda (expression state)
    (cond
      ((equal? expression 'true)                                    'true)
      ((equal? expression 'false)                                   'false)
      ((contains? (get-operator expression) keyword-bool-operators) (m-state-bool-operators   expression state))
      ((contains? (get-operator expression) keyword-comparators)    (m-state-comparators      expression state))
      (else                                                         (error "This isn't a boolean expression")))))