#lang racket
(require "functionParser.rkt")



(define KEYWORD_MATH_OPERATORS '(+ - * / %))
(define KEYWORD_BOOL_OPERATORS '(&& || !))
(define KEYWORD_COMPARATORS    '(== != < > <= >=))
(define KEYWORD_CONTROL        '(var = return if while begin try catch finally break continue throw function funcall))
(define EMPTY_STATE            '((() ())))
(define EMPTY_ENVIRONMENT      '(() ()))

(define NULL 'null)
(define TRUE 'true)
(provide TRUE)
(define FALSE 'false)
(provide FALSE)

(define MAIN_CALL '(funcall main ()))

; === Main ===
; Interpreter entry point. Reads a file as a program and interprets it, returning the return value of the program
(provide interpret)
(define interpret
  (lambda (filename)
    (m-state-body (parser filename)
                  EMPTY_STATE
                  (lambda (s) (interpret-return-output (m-value-function MAIN_CALL s)))
                  break-error
                  continue-error
                  (lambda (s v) (error "Returned outside of a function"))
                  (lambda (s v) (error "Error thrown without catch")))))

(define break-error
  (lambda (s) (error "Break outside of loop")))

(define continue-error
  (lambda (s) (error "Continue outside of loop")))

(define identity
  (lambda (v) v))

(define interpret-return-output
  (lambda (return)
    (cond
      ((eq? return #t) TRUE)
      ((eq? return #f) FALSE)
      (else            return))))

; === State helper functions ===
(define (get-current-environment state)
  (car state))

(define (get-next-environments state)
  (cdr state))

(define (update-current-environment state new-environment)
  (cons new-environment (cdr state)))

(define (get-current-scope environment)
  (cons (car environment) (cons (cadr environment) '())))

(define (next-scopes environment)
  (cddr environment))

(define (add-environment state)
  (cons EMPTY_ENVIRONMENT state))

(define (add-scope state)
  (update-current-environment state (add-scope-cps (get-current-environment state) identity)))

(define (add-scope-cps environment continuation)
  (if (null? (next-scopes environment))
      (continuation (append environment EMPTY_ENVIRONMENT))
      (add-scope-cps (next-scopes environment) (lambda (v) (continuation (append (get-current-scope environment) v))))))

(define (next-next-scopes environment)
  (cddddr environment))

(define (remove-scope state)
  (update-current-environment state (remove-scope-cps (get-current-environment state) identity)))

(define (remove-scope-cps state continuation)
  (if (null? (next-scopes state))
      (error "trying to leave scope without entering one")
  
      (if (null? (next-next-scopes state))
          (continuation (get-current-scope state))
          (remove-scope-cps (next-scopes state) (lambda (v) (continuation (append (get-current-scope state) v)))))))

(define contains?
  (lambda (element list)
    (cond
      ((null? list)             #f)
      ((eq? (car list) element) #t)
      (else                     (contains? element (cdr list))))))

(define (get-environment-names environment)
  (get-environment-names-cps environment identity))

(define (get-environment-names-cps environment continuation)
  (if (null? environment)
      (continuation '())
      (get-environment-names-cps (next-scopes environment) (lambda (v) (continuation (append (car environment) v))))))

(define (get-environment-values environment)
  (get-environment-values-cps environment identity))

(define (get-environment-values-cps state continuation)
  (if (null? state)
      (continuation '())
      (get-environment-values-cps (next-scopes state) (lambda (v) (continuation (append (cadr state) v))))))

(define make-environment
  (lambda (names values)
    (list names values)))

(define (is-last-scope? environement)
  (and (list? (car environement)) (list? (cadr environement)) (null? (cddr environement))))

; Check whether a name is bound
(define check-for-binding
  (lambda (name state)
    (check-for-binding-cps name state identity)))

(define check-for-binding-cps
  (lambda (name state return)
    (cond
      ((null? state)                                       (return #f))
      ((check-for-binding-in-environment name (get-current-environment state)) (return #t))
      (else                                                (check-for-binding-cps name (get-next-environments state) return))))) 

(define check-for-binding-in-environment
  (lambda (name environment)
    (contains? name (get-environment-names environment))))
 
; Find the value for a bound name
(define get-binding-value
  (lambda (name state)
    (get-binding-value-cps name state identity)))

(define get-binding-value-cps
  (lambda (name state return)
    (cond
      ((check-for-binding-in-environment name (get-current-environment state)) (return (get-binding-value-environment name (get-current-environment state))))
      (else (get-binding-value-cps name (get-next-environments state) return)))))

(define get-binding-value-environment
  (lambda (name environment)
    (cond
      ((null? environment)                                  (error "Name not bound"))
      ((and (eq? (car (get-environment-names environment)) name) (eq? (car (get-environment-values environment)) NULL))
       (error "Tried to evaluate expression with an uninitialized variable"))
      ((eq? (car (get-environment-names environment)) name) (car (get-environment-values environment)))
      (else                                                 (get-binding-value-environment name (make-environment (cdr (get-environment-names environment)) (cdr (get-environment-values environment))))))))

; helper
(define (state-length state)
  (state-length-cps (car state) identity))

(define (state-length-cps list return)
  (if (null? list)
      (return 0)
      (state-length-cps (cdr list) (lambda (v) (return (+ 1 v))))))

(define (get-environment-count state)
  (get-environement-count-cps state identity))

(define (get-environement-count-cps state return)
  (if (null? state)
      (return 0)
      (get-environement-count-cps (get-next-environments state) (lambda (c) (return (+ c 1))))))

; Add a name-value pair binding to the state, or replce the value if name is already bound
(define create-new-binding
  (lambda (name value state)
    (update-current-environment state (create-new-binding-cps name value (get-current-environment state) identity))))

(define create-new-binding-cps
  (lambda (name value environment return)
    (if (is-last-scope? environment)
        (create-new-binding-in-scope-cps name value (get-current-scope environment) return)
        (create-new-binding-cps name value (next-scopes environment) (lambda (e) (return (append (get-current-scope environment) e)))))))

(define create-new-binding-in-scope-cps
  (lambda (name value scope return)
    (return (make-environment (append (get-environment-names scope) (list name)) (append (get-environment-values scope) (list value))))))

(define update-binding
  (lambda (name value state)
    (update-binding-cps name value state identity)))


(define update-binding-cps
  (lambda (name value state return)
    (if (check-for-binding-in-environment name (get-current-environment state))
        (update-binding-in-environment-cps name value (get-current-environment state) (lambda (e) (return (cons e (get-next-environments state)))))
        (update-binding-cps name value (get-next-environments state) (lambda (s) (return (cons (get-current-environment state) s)))))))
    
(define update-binding-in-environment-cps
  (lambda (name value environment return)
    (cond
      ((null? environment) (error "Tried to update a binding that doesn't exist"))
      ((not (contains? name (get-environment-names (get-current-scope environment)))) (update-binding-in-environment-cps name value (next-scopes environment)
                                                                                                                         (lambda (v) (return (append (get-current-scope environment) v)))))
      (else (update-binding-cps-scope name value (get-current-scope environment)
                                      (lambda (v) (return (append v (next-scopes environment)))))))))
      
(define (update-binding-cps-scope name value state return)
  (cond
    ((eq? (car (get-environment-names (get-current-scope state))) name) (return (append (make-environment (get-environment-names (get-current-scope state))
                                                                                                          (cons value (cdr (get-environment-values (get-current-scope state))))) (next-scopes state))))
    
      
    (else (update-binding-cps-scope name value (append (make-environment (cdr (get-environment-names (get-current-scope state)))
                                                                         (cdr (get-environment-values (get-current-scope state)))) (next-scopes state))
                                    (lambda (v) (return (make-environment
                                                         (cons (car (get-environment-names  (get-current-scope state))) (get-environment-names  (get-current-scope v)))
                                                         (cons (car (get-environment-values (get-current-scope state))) (get-environment-values (get-current-scope v))))))))))

; Get the keyword the defines the statement type from a statement represented by a list
(define get-statement-type
  (lambda (statement)
    (car statement)))

; === State handler ===
; Modify the state by a statement
(define m-state
  (lambda (statement state next break continue return throw)
    (cond
      ((null? statement)                                                 (next state))
      ((eq? statement NULL)                                              (next state))
      ((single-element? statement)                                       (next state))
      ((contains? (get-statement-type statement) KEYWORD_MATH_OPERATORS) (m-state-operators statement state next break continue return throw))
      ((contains? (get-statement-type statement) KEYWORD_BOOL_OPERATORS) (m-state-operators statement state next break continue return throw))
      ((contains? (get-statement-type statement) KEYWORD_COMPARATORS)    (m-state-operators statement state next break continue return throw))
      ((contains? (get-statement-type statement) KEYWORD_CONTROL)        (m-state-control   statement state next break continue return throw))
      (else                                                              (error "Unknown keyword")))))

(define m-state-operators
  (lambda (statement state next break continue return throw)
    (if (second-operand-exists? statement)
        (m-state-operators-two statement state next break continue return throw)
        (m-state-operators-one statement state next break continue return throw))))

(define m-state-operators-one
  (lambda (statement state next break continue return throw)
    (m-state (get-first-operand statement) state next break continue return throw)))

(define m-state-operators-two
  (lambda (statement state next break continue return throw)
    (m-state (get-first-operand statement) state (lambda (s) (m-state (get-second-operand statement) s next break continue return throw))
             break continue return throw)))

; === Control flow state handlers ===
(define m-state-control
  (lambda (statement state next break continue return throw)
    (cond
      ((eq? (get-statement-type statement) 'var)      (m-state-var               statement state next break continue return throw))
      ((eq? (get-statement-type statement) 'return)   (m-state-return            statement state next break continue return throw))
      ((eq? (get-statement-type statement) '=)        (m-state-assign            statement state next break continue return throw))
      ((eq? (get-statement-type statement) 'if)       (m-state-if                statement state next break continue return throw))
      ((eq? (get-statement-type statement) 'while)    (m-state-while             statement state next break continue return throw))
      ((eq? (get-statement-type statement) 'begin)    (m-state-begin             statement state next break continue return throw))
      ((eq? (get-statement-type statement) 'try)      (m-state-try-catch-finally statement state next break continue return throw))
      ((eq? (get-statement-type statement) 'continue) (m-state-continue          statement state next break continue return throw))
      ((eq? (get-statement-type statement) 'break)    (m-state-break             statement state next break continue return throw))
      ((eq? (get-statement-type statement) 'throw)    (m-state-throw             statement state next break continue return throw))
      ((eq? (get-statement-type statement) 'function) (m-state-function          statement state next break continue return throw))
      ((eq? (get-statement-type statement) 'funcall)  (m-state-funcall           statement state next break continue return throw))
      (else                                         (error "Unknown Control Keyword")))))

; m-state-body-begin
(define (m-state-body-begin statementlist state next break continue return throw)
  (if (null? statementlist)
      (next state)
      (m-state-body statementlist (add-scope state)
                    (lambda (s) (next (remove-scope s)))
                    (lambda (s) (break (remove-scope s)))
                    (lambda (s) (continue (remove-scope s)))
                    (lambda (s v) (return (remove-scope s) v))
                    (lambda (s v) (throw (remove-scope s) v)))))

(define (one-statement? statementlist)
  (or (not (list? (car statementlist))) (null? (cdr statementlist))))

(define (m-state-body statementlist state next break continue return throw)
  (if (one-statement? statementlist)
      (if (not (list? (car statementlist)))
          (m-state statementlist state next break continue return throw)
          (m-state  (car statementlist) state next break continue return throw))
      (m-state (car statementlist) state (lambda (s) (m-state-body (cdr statementlist) s next break continue return throw))
               break continue return throw)))


; begin/ {} statement handler
(define (m-state-begin statement state next break continue return throw)
  (m-state-body-begin (get-statement-list statement) state next break continue return throw))

(define (get-statement-list statement)
  (cdr statement))

; try statement handler
(define catch-exist?
  (lambda (statement)
    (eq? 'catch (caaddr statement))))

(define finally-exist?
  (lambda (statement)
    (if (null? (cadddr statement))
        (eq? 'finally (caaddr statement))
        (eq? 'finally (car (cadddr statement))))))

(define get-try-block-statement-list
  (lambda (statement)
    (cadr statement)))

(define (get-catch-block-statement-list statement)
  (if catch-exist?
      (caddr (caddr statement))
      '()))

(define (get-finally-block-statement-list statement)
  (if (finally-exist? statement)
      (if (null? (cdddr statement))
          (caadr (caddr statement))
          (cadar (cdddr statement)))
      '()))

(define get-catch-exception-name
  (lambda (statement)
    (caadr (caddr statement))))

(define m-state-try-catch-finally
  (lambda (statement state next break continue return throw)
    (m-state-try (get-try-block-statement-list statement) state
                 (lambda (s) (m-state-finally (get-finally-block-statement-list statement) s next break continue return throw)) ; new next
                 (lambda (s) (m-state-finally (get-finally-block-statement-list statement) s break break continue return throw)); new break
                 (lambda (s) (m-state-finally (get-finally-block-statement-list statement) s continue break continue return throw)); new continue
                 (lambda (s1 v) (m-state-finally (get-finally-block-statement-list statement) s1 (lambda (s2) (return s2 v)) break continue return throw))
                 (lambda (s1 v) (m-state-catch (get-catch-block-statement-list statement) (create-new-binding (get-catch-exception-name statement) v s1)
                                               (lambda (s2) (m-state-finally (get-finally-block-statement-list statement) s2 next break continue return throw))
                                               (lambda (s2) (m-state-finally (get-finally-block-statement-list statement) s2 break break continue return throw))
                                               (lambda (s2) (m-state-finally (get-finally-block-statement-list statement) s2 continue break continue return throw))
                                               (lambda (s2 v2) (m-state-finally (get-finally-block-statement-list statement) s2 (lambda (s3) (return s3 v2)) break continue return throw))
                                               (lambda (s2 v2) (m-state-finally (get-finally-block-statement-list statement) s2 (lambda (s3) (throw s3 v2)) break continue return throw)))))))
     
;; try block handler
(define m-state-try
  (lambda (statementlist state next break continue return throw)
    (m-state-body-begin statementlist state next break continue return throw)))

;; catch block handler
(define m-state-catch
  (lambda (statementlist state next break continue return throw)
    (m-state-body-begin statementlist state next break continue return throw)))

;; finally block handler
(define m-state-finally
  (lambda (statementlist state next break continue return throw)
    (m-state-body-begin statementlist state next break continue return throw)))

; continue
(define m-state-continue
  (lambda (statementlist state next break continue return throw)
    (continue state)))

; break
(define m-state-break
  (lambda (statementlist state next break continue return throw)
    (break state)))

; throw
(define get-throw-value
  (lambda (statement state)
    (m-value (cadr statement) state)))

(define m-state-throw
  (lambda (statementlist state next break continue return throw)
    (throw state (get-throw-value statementlist state))))

; if statement handler
(define (get-condition statement)
  (cadr statement))

(define (get-then statement)
  (caddr statement))

(define (get-else statement)
  (cadddr statement))

(define (get-else-not-exist statement)
  (cdddr statement))

(define (else-exist? statement)
  (not (null? (get-else-not-exist statement))))

(define (m-state-if statement state next break continue return throw)
  (if (m-bool (get-condition statement) state)
      (m-state (get-condition statement) state (lambda (s) (m-state-body-begin (get-then statement) s next break continue return throw))
               break continue return throw)
      
      (if (else-exist? statement)
          (m-state (get-condition statement) state (lambda (s) (m-state-body-begin (get-else statement) s next break continue return throw))
                   break continue return throw)
          (m-state (get-condition statement) state next break continue return throw))))

; while statement handler
(define (m-state-while statement state next break continue return throw)
  (if (m-bool (get-condition statement) state)
      (m-state (get-condition statement) state (lambda (s1)
                                                 (m-state-body-begin (get-loop-body statement) s1 (lambda (s2)
                                                                                                    (m-state-while statement s2 next break continue return throw))
                                                                     next (lambda (s3)
                                                                            (m-state-while statement s3 next break continue return throw)) return throw))
               break continue return throw)
      (m-state (get-condition statement) state next break continue return throw)))

(define get-loop-body
  (lambda (statement)
    (caddr statement)))


; var statement handler
(define m-state-var
  (lambda (statement state next break continue return throw)
    (if (check-for-binding-in-environment (get-var-name statement) (get-current-environment state))
        (error "Variable Already Declared")
        (m-state (get-var-expression statement) state (lambda (s) (next (create-new-binding (get-var-name statement) (get-var-value statement s) s))) break continue return throw))))

(define get-var-name
  (lambda (statement)
    (cadr statement)))

(define get-var-expression
  (lambda (statement)
    (if (null? (cddr statement))
        NULL
        (caddr statement))))

(define get-var-value
  (lambda (statement state)
    (if (null? (cddr statement))
        NULL
        (m-value (get-var-expression statement) state))))

; return statement handler
(define m-state-return
  (lambda (statement state next break continue return throw)
    (return state (get-return-value statement state))))

(define get-return-value
  (lambda (statement state)
    (m-value (cadr statement) state)))

; assign statement handler
(define m-state-assign
  (lambda (statement state next break continue return throw)
    (if (check-for-binding (get-assign-name statement) state)
        (m-state (get-second-operand statement) state (lambda (s) (next (update-binding (get-assign-name statement) (get-assign-value statement state) s))) break continue return throw)
        (error "Undeclared Variable"))))

(define get-assign-name
  (lambda (statement)
    (cadr statement)))

(define get-assign-value
  (lambda (statement state)
    (m-value (get-second-operand statement) state)))

; function defenition handler
(define m-state-function
  (lambda (statement state next break continue return throw)
    (next (create-new-binding (get-function-name statement)
                       (make-closure (get-function-variables statement) (get-function-body statement) state)
                       state))))

(define make-closure
  (lambda (formal-parameters body state)
    (list formal-parameters body
          (lambda (s) (truncate-state-to-match state s)))))

(define get-function-name
  (lambda (statement)
    (cadr statement)))

(define get-function-variables
  (lambda (statement)
    (caddr statement)))

(define get-function-body
  (lambda (statement)
    (cadddr statement)))

(define (next-environment state)
  (cdr state))

(define truncate-state-to-match
  (lambda (main-state truncate-state)
    (truncate-state-to-length-cps truncate-state (get-environment-count main-state) (lambda (s l) s))))

(define truncate-state-to-length-cps
  (lambda (truncate-state length return)
    (if (null? truncate-state)
        (return truncate-state length)
        (truncate-state-to-length-cps
         (next-environment truncate-state)
         length
         (lambda (s l) (if (eq? l 0) (return s 0) (return (cons (get-current-environment truncate-state) s) (- l 1))))))))


    


; funciton call handler
(define m-state-funcall
  (lambda (statement state next break continue return throw)
    (m-state-body
     (get-closure-body (get-binding-value (get-funcall-name statement) state))
     (bind-parameters
      (get-closure-params (get-binding-value (get-funcall-name statement) state))
      (get-funcall-args statement)
      (add-environment ((get-closure-environment (get-binding-value (get-funcall-name statement) state)) state))
      state)
     (lambda (s) (next (recover-state s state)))
     break-error
     continue-error
     (lambda (s v) (next (recover-state s state)))
     (lambda (s v) (throw (recover-state s state))))))

(define get-funcall-name
  (lambda (statement)
    (cadr statement)))

(define get-funcall-args
  (lambda (statement)
    (cddr statement)))

(define get-closure-params
  (lambda (closure)
    (car closure)))

(define get-closure-body
  (lambda (closure)
    (cadr closure)))

(define get-closure-environment
  (lambda (closure)
    (caddr closure)))
    
(define bind-parameters
  (lambda (params args new-state old-state)
    (if (null? params)
        new-state
        (bind-parameters (cdr params) (cdr args) (create-new-binding (car params) (m-value (car args) old-state) new-state) old-state))))


(define recover-state
  (lambda (inner-state outer-state)
    (truncate-state-to-match outer-state inner-state)))

;=====================================================
;=====================================================
;=====================================================
;=====================================================
; === Values expression evaluator ===
(define m-value
  (lambda (expression state)
    (cond
      ((is-function-expression? expression state) (m-value-function expression state))
      ((is-bool-expression? expression state)     (m-bool expression state))
      (else                                       (m-number expression state)))))

; Check to see if we need to evaluate a function or not
(define is-function-expression?
  (lambda (expression state)
    (and (list? expression) (eq? (get-operator expression) 'funcall))))

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
    (not (null? (cddr expression)))))

(define (single-element? expression)
  (not (list? expression)))

(define is-bool-expression?
  (lambda (expression state)
    (cond
      ((eq? expression 'true)                                       #t)
      ((eq? expression 'false)                                      #t)
      ((number? expression)                                         #f)
      ((check-for-binding expression state)                         (boolean? (get-binding-value expression state)))
      ((single-element? expression)                                 #f)
      ((contains? (get-operator expression) KEYWORD_BOOL_OPERATORS) #t)
      ((contains? (get-operator expression) KEYWORD_COMPARATORS)    #t)
      (else                                                         #f))))

; === Function expression evaluator ===
(define m-value-function
  (lambda (expression state)      
    (m-state-body
     (get-closure-body (get-binding-value (get-funcall-name expression) state))
     (bind-parameters
      (get-closure-params (get-binding-value (get-funcall-name expression) state))
      (get-funcall-args expression)
      (add-environment ((get-closure-environment (get-binding-value (get-funcall-name expression) state)) state))
      state)
     (lambda (s) (error "You can't use the return value from a void function."))
     break-error
     continue-error
     (lambda (s v) v)
     (lambda (s v) (error "How did we get here.")))))

; === Numerical expression evaluator ===
(define m-number
  (lambda (expression state)
    (cond
      ((is-bool-expression? expression state)                       (if (m-bool expression state) 1 0))  ; Cast bool to number
      ((number? expression)                                         expression)
      ((check-for-binding expression state)                         (get-binding-value expression state))
      ((single-element? expression)                                 (error "Undeclared Variable"))
      ((contains? (get-operator expression) KEYWORD_MATH_OPERATORS) (m-number-math-operators expression state))
      ((eq? (get-operator expression) '=)                           (m-number-assign expression state))
      ((eq? (get-operator expression) 'funcall)                      (m-number (m-value-function expression state) state))
      (else                                                         (error "This isn't a numerical expression")))))


; === Numerical expression evaluators ===
(define m-number-math-operators
  (lambda (expression state)
    (cond
      ((eq? (get-operator expression) '+) (m-number-addition       expression state))
      ((eq? (get-operator expression) '-) (m-number-subtraction    expression state))
      ((eq? (get-operator expression) '*) (m-number-multiplication expression state))
      ((eq? (get-operator expression) '/) (m-number-division       expression state))
      ((eq? (get-operator expression) '%) (m-number-modulus        expression state)))))

(define (m-number-helper func expression state)
  (func
   (m-number (get-first-operand expression) state)
   (m-number (get-second-operand expression) (m-state (get-first-operand expression) state
                                                     identity
                                                     (lambda (s) (error "Called break in expression"))
                                                     (lambda (s) (error "Called continue in expression"))
                                                     (lambda (s) (error "Called return in expression"))
                                                     (lambda (s) (error "Called throw in expression"))))))
   
; addition expression evaluator
(define m-number-addition
  (lambda (expression state)
    (m-number-helper + expression state)))
; subtraction expresion evaluator
(define m-number-subtraction
  (lambda (expression state)
    (if (second-operand-exists? expression)
        ; Subtraction
        (m-number-helper - expression state)
        ; Unary
        (- 0 (m-value (get-first-operand expression) state)))))

; multiplication expresion evaluator
(define m-number-multiplication
  (lambda (expression state)
    (m-number-helper * expression state)))

; division expresion evaluator
(define m-number-division
  (lambda (expression state)
    (m-number-helper quotient expression state)))

; modulus expresion evaluator
(define m-number-modulus
  (lambda (expression state)
    (m-number-helper modulo expression state)))


; assignment expression evaluator
(define m-number-assign
  (lambda (expression state)
    (get-assign-value expression state)))

; === Boolean expression evaluator ===
(define m-bool
  (lambda (expression state)
    (cond
      ((equal? expression 'true)                                    #t)
      ((equal? expression 'false)                                   #f)
      ((check-for-binding expression state)                         (get-binding-value      expression state))
      ((single-element? expression)                                 (error "Undeclared Variable"))
      ((contains? (get-operator expression) KEYWORD_BOOL_OPERATORS) (m-bool-bool-operators expression state))
      ((contains? (get-operator expression) KEYWORD_COMPARATORS)    (m-bool-comparators    expression state))
      ((eq? (get-operator expression) 'funcall)                     (m-bool (m-value-function expression state) state))
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
    (m-number-helper equal? expression state)))

; not equals expression evaluator
(define m-bool-not-equals
  (lambda (expression state)
    (not (m-number-helper equal? expression state))))


; less than expression evaluator
(define m-bool-less-than
  (lambda (expression state)
    (m-number-helper < expression state)))


; greater than expression evaluator
(define m-bool-greater-than
  (lambda (expression state)
    (m-number-helper > expression state)))


; less than or equal to expression evaluator
(define m-bool-less-than-equals
  (lambda (expression state)
    (m-number-helper <= expression state)))
       

; greater than or equal to expression evaluator
(define m-bool-greater-than-equals
  (lambda (expression state)
    (m-number-helper >= expression state)))

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
    (not (m-value (get-first-operand expression) state))))


(define (m-bool-helper func expression state)
  (func
   (m-bool (get-first-operand expression) state)
   (m-bool (get-second-operand expression) (m-state (get-first-operand expression) state
                                                     identity
                                                     (lambda (s) (error "Called break in expression"))
                                                     (lambda (s) (error "Called continue in expression"))
                                                     (lambda (s) (error "Called return in expression"))
                                                     (lambda (s) (error "Called throw in expression"))))))

; and expression handler
(define m-bool-and
  (lambda (expression state)
    (m-bool-helper (lambda (a b) (and a b)) expression state)))

; or expression handler
(define m-bool-or
  (lambda (expression state)
    (m-bool-helper (lambda (a b) (or a b)) expression state)))

;(interpret "test-cases/given-tests/part3-test/test04.txt")
