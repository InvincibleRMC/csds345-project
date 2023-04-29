#lang racket
(require "simpleParser.rkt")

(define KEYWORD_MATH_OPERATORS '(+ - * / %))
(define KEYWORD_BOOL_OPERATORS '(&& || !))
(define KEYWORD_COMPARATORS    '(== != < > <= >=))
(define KEYWORD_CONTROL        '(var = return if while begin))
(define EMPTY_STATE            '(() ()))

(define NULL 'null)
(define RETURN 'return)
(define TRUE 'true)
(provide TRUE)
(define FALSE 'false)
(provide FALSE)

; === Main ===
; Interpreter entry point. Reads a file as a program and interprets it, returning the return value of the program
(provide interpret)
(define interpret
  (lambda (filename)
    (m-state-body (parser filename)
                  EMPTY_STATE
                  (lambda (s) (run-main-method (find-main-closure (get-current-scope (get-current-environment s))) s))
                  break-error
                  continue-error
                  (lambda (s v) (error "Returned outside of a function"))
                  (lambda (s v) (error "Error thrown without catch"))
                  '())))

(define run-main-method
  (lambda (main-closure state)
    (m-state-body
     (get-closure-body main-closure)
     state
     (lambda (s) (error "No return statement in main function"))
     break-error
     continue-error
     (lambda (s v) (interpret-return-output v))
     (lambda (s v) (error "Uncaught exception"))
     (find-main-class (get-current-scope (get-current-environment state))))))

(define find-main-class
  (lambda (scope)
    (car (find-main-cps scope identity))))

(define find-main-closure
  (lambda (scope)
    (cadr (find-main-cps scope identity))))

(define find-main-cps
  (lambda (scope return)
    (if (null? (get-scope-names scope))
        (error "No main function")
        (find-main-helper-cps
         (get-class-scope (car (get-scope-values scope)))
         (lambda (closure) (return (list (car (get-scope-names scope)) closure)))
         (lambda () (find-main-cps (get-next-scope-elements scope) return)))))) 

(define find-main-helper-cps
  (lambda (class-scope return next)
    (cond
      ((equal? class-scope EMPTY_ENVIRONMENT)         (next))
      ((eq? (car (get-scope-names class-scope)) MAIN) (return (car (get-scope-values class-scope))))
      (else                                           (find-main-helper-cps (get-next-scope-elements class-scope) return next)))))

(define break-error
  (lambda (s) (error "Break outside of loop")))

(define continue-error
  (lambda (s) (error "Continue outside of loop")))

(define identity
  (lambda (v) v))

(define interpret-return-output
  (lambda (return)
    (cond
      ((eq? return #t) 'true)
      ((eq? return #f) 'false)
      (else            return))))

; === State helper functions ===

(define (get-current-state state)
  (cons (car state) (cons (cadr state) '())))

(define (next-state state)
  (cddr state))

(define (add-scope state)
  (add-scope-cps state (lambda (v) v)))

(define (add-scope-cps state continuation)
  (if (null? (next-state state))
      (continuation (append state EMPTY_STATE))
      (add-scope-cps (next-state state) (lambda (v) (continuation (append (get-current-state state) v))))))

(define (next-next-state state)
  (cddddr state))

(define (remove-scope state)
  (remove-scope-cps state (lambda (v) v)))

(define (remove-scope-cps state continuation)
  (if (null? (next-state state))
      (error "trying to leave scope without entering one")
  
      (if (null? (next-next-state state))
          (continuation (get-current-state state))
          (remove-scope-cps (next-state state) (lambda (v) (continuation (append (get-current-state state) v)))))))

(define contains?
  (lambda (element list)
    (cond
      ((null? list)             #f)
      ((eq? (car list) element) #t)
      (else                     (contains? element (cdr list))))))

(define (get-state-names state)
  (get-state-names-cps state (lambda (v) v)))

(define (get-state-names-cps state continuation)
  (if (null? state)
      (continuation '())
      (get-state-names-cps (next-state state) (lambda (v) (continuation (append (car state) v))))))

(define (get-state-values state)
  (get-state-values-cps state (lambda (v) v)))

(define (get-state-values-cps state continuation)
  (if (null? state)
      (continuation '())
      (get-state-values-cps (next-state state) (lambda (v) (continuation (append (cadr state) v))))))

(define make-state
  (lambda (names values)
    (list names values)))

;(define (is-empty-state? state)
  ;(state-length state)

(define (is-last-state? state)
  (and (list? (car state)) (list? (cadr state)) (null? (cddr state))))

;(is-last-state? (append EMPTY_STATE '((b c d) (2 3 4))))
;(is-last-state? (remove-scope(append EMPTY_STATE '((b c d) (2 3 4)))))
; Check whether a name is bound
(define check-for-binding
  (lambda (name state)
    (contains? name (get-state-names state))))

; Find the value for a bound name
(define get-binding-value
  (lambda (name state type)
    (get-binding-value-cps name state identity)))

(define get-binding-value-cps
  (lambda (name state return)
    (cond
      ((null? state)                            (error "Name not bound"))
      ((and (eq? (car (get-state-names state)) name) (eq? (car (get-state-values state)) NULL))
                                                (error "Tried to evaluate expression with an uninitialized variable"))
      ((eq? (car (get-state-names state)) name) (car (get-state-values state)))
      (else                                     (get-binding-value name (make-state (cdr (get-state-names state)) (cdr (get-state-values state))))))))

; helper
(define (state-length state)
  (state-length-cps (car state) (lambda (v) v)))

(define (state-length-cps list return)
  (if (null? list)
      (return 0)
      (state-length-cps (cdr list) (lambda (v) (return (+ 1 v))))))

; Add a name-value pair binding to the state, or replce the value if name is already bound
(define add-binding
  (lambda (name value state)
    (add-binding-cps name value state (lambda (v) v))))

(define add-binding-cps
  (lambda (name value state return)
    (cond
      ((and (= 0 (state-length state))
            (is-last-state? state))         (return (make-state (list name) (list value))))
      ((and (not (is-last-state? state))
            (not (contains? name (get-state-names (get-current-state state)))))
                                            (add-binding-cps name value (next-state state) (lambda (v) (return (append (get-current-state state) v)))))
      ((eq? (car (get-state-names (get-current-state state))) name) (return (append (make-state (get-state-names state) (cons value (cdr (get-state-values state)))) (next-state state))))
      (else                                 (add-binding-cps name value (append (make-state (cdr (get-state-names (get-current-state state)))
                                                                        (cdr (get-state-values (get-current-state state)))) (next-state state))
                                                                                (lambda (v) (return (make-state
                                                                                    (cons (car (get-state-names  (get-current-state state))) (get-state-names  v))
                                                                                    (cons (car (get-state-values (get-current-state state))) (get-state-values v))))))))))

; Get the keyword the defines the statement type from a statement represented by a list
(define get-statement-type
  (lambda (statement)
    (car statement)))

; replace the last environments of outer states with the environments of inner state
(define recover-state
  (lambda (inner-state outer-state instance-name)
    (recover-this instance-name inner-state (recover-state-cps (get-next-environments inner-state)
                       outer-state
                       (- (get-environment-count outer-state) (get-environment-count (get-next-environments  inner-state))) identity))))

(define recover-state-cps
  (lambda (inner-state outer-state skip-count return)
    (cond
      ((null? inner-state) (return '()))
      ((> skip-count 0)    (recover-state-cps inner-state (cdr outer-state) (- skip-count 1) (lambda (s) (return (cons (car outer-state) s)))))
      (else                (return inner-state)))))

(define recover-this
  (lambda (instance-name inner-state merged-state)
    (if (null? instance-name)
        merged-state
        (update-binding instance-name (get-binding-value 'this inner-state) merged-state))))

; === Class helper functions ===
(define make-class-closure
  (lambda (extends body name)
    (list
     (if (null? extends) '() (cadr extends))
     (get-current-scope (get-current-environment
                         (m-state-body
                          body
                          EMPTY_STATE
                          identity
                          break-error
                          continue-error
                          (lambda (s v) (error "return statement outside of method"))
                          (lambda (s v) (error "throw statement outside of method"))
                          name))))))

(define get-class-super-type
  (lambda (closure)
    (car closure)))

(define get-class-scope
  (lambda (closure)
    (cadr closure)))

(define make-instance-closure
  (lambda (type class-closure state)
    (list type (make-instance-closure-environment class-closure state))))

(define make-instance-closure-environment
  (lambda (class-closure state)
    (if (null? (get-class-super-type class-closure))
        (get-class-scope class-closure)
        (append (get-class-scope class-closure) (make-instance-closure-environment (get-binding-value (get-class-super-type class-closure) state '()) state)))))

(define get-instance-type
  (lambda (closure)
    (car closure)))

(define get-instance-environment
  (lambda (closure)
    (cadr closure)))

(define update-instance-environment
  (lambda (closure environment)
    (list (get-instance-type closure) environment)))

; === State handler ===
; Modify the state by a statement
(define m-state
  (lambda (statement state next break continue return throw type)
    (cond
      ((null? statement)                                                 (next state))
      ((eq? statement NULL)                                              (next state))
      ((single-element? statement)                                       (next state))
      ((contains? (get-statement-type statement) KEYWORD_MATH_OPERATORS) (m-state-operators statement state next break continue return throw type))
      ((contains? (get-statement-type statement) KEYWORD_BOOL_OPERATORS) (m-state-operators statement state next break continue return throw type))
      ((contains? (get-statement-type statement) KEYWORD_COMPARATORS)    (m-state-operators statement state next break continue return throw type))
      ((contains? (get-statement-type statement) KEYWORD_CONTROL)        (m-state-control   statement state next break continue return throw type))
      (else                                                              (error "Unknown keyword")))))

(define m-state-operators
  (lambda (statement state next break continue return throw)
    (if (second-operand-exists? statement)
        (m-state-operators-two statement state)
        (m-state-operators-one statement state))))

(define m-state-operators-one
  (lambda (statement state)
    (m-state (get-first-operand statement) state)))

(define m-state-operators-two
  (lambda (statement state)
    (m-state (get-second-operand statement) (m-state (get-first-operand statement) state))))

; === Control flow state handlers ===
(define m-state-control
  (lambda (statement state next break continue return throw type)
    (cond
      ((eq? (get-statement-type statement) 'var)             (m-state-var               statement state next break continue return throw type))
      ((eq? (get-statement-type statement) 'return)          (m-state-return            statement state next break continue return throw type))
      ((eq? (get-statement-type statement) '=)               (m-state-assign            statement state next break continue return throw type))
      ((eq? (get-statement-type statement) 'if)              (m-state-if                statement state next break continue return throw type))
      ((eq? (get-statement-type statement) 'while)           (m-state-while             statement state next break continue return throw type))
      ((eq? (get-statement-type statement) 'begin)           (m-state-begin             statement state next break continue return throw type))
      ((eq? (get-statement-type statement) 'try)             (m-state-try-catch-finally statement state next break continue return throw type))
      ((eq? (get-statement-type statement) 'continue)        (m-state-continue          statement state next break continue return throw type))
      ((eq? (get-statement-type statement) 'break)           (m-state-break             statement state next break continue return throw type))
      ((eq? (get-statement-type statement) 'throw)           (m-state-throw             statement state next break continue return throw type))
      ((eq? (get-statement-type statement) 'function)        (m-state-function          statement state next break continue return throw type))
      ((eq? (get-statement-type statement) 'static-function) (m-state-function          statement state next break continue return throw type))
      ((eq? (get-statement-type statement) 'funcall)         (m-state-funcall           statement state next break continue return throw type))
      ((eq? (get-statement-type statement) 'class)           (m-state-class             statement state next break continue return throw type))
      ((eq? (get-statement-type statement) 'new)             (m-state-new               statement state next break continue return throw type))
      ((eq? (get-statement-type statement) 'dot)             (m-state-dot               statement state next break continue return throw type))
      (else                                                  (error "Unknown Control Keyword")))))



(define (m-state-body statementlist state next break continue return throw type)
  (if (one-statement? statementlist)
      (if (not (list? (car statementlist)))
          (m-state statementlist state next break continue return throw)
          (m-state  (car statementlist) state next break continue return throw type))
      (m-state (car statementlist) state (lambda (s) (m-state-body (cdr statementlist) s next break continue return throw type))
               break continue return throw type)))


; begin/ {} statement handler
(define (m-state-begin statement state)
     (remove-scope (m-state-body (get-statement-list statement) (add-scope state))))
       

   ;
   (define (get-statement-list statement)
     (cdr statement))

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

(define (m-state-if statement state)
  (if (m-bool (get-condition statement) state)
      (m-state (get-then statement) (m-state (get-condition statement) state))
      
      (if (else-exist? statement)
          (m-state (get-else statement) (m-state (get-condition statement) state))
          (m-state (get-condition statement) state))))

; while statement handler
(define (m-state-while statement state)
  (if (m-bool (get-condition statement) state)
      (m-state-while statement (m-state (get-loop-body statement) (m-state (get-condition statement) state)))
      (m-state (get-condition statement) state)))

(define get-loop-body
  (lambda (statement)
    (caddr statement)))


; var statement handler
(define m-state-var
  (lambda (statement state next break continue return throw type)
    (if (check-for-binding-in-environment (get-var-name statement) (get-current-environment state))
        (error "Variable Already Declared")
        (m-state (get-var-expression statement) state (lambda (s) (next (create-new-binding (get-var-name statement) (get-var-value statement s type) s))) break continue return throw type))))

(define get-var-name
  (lambda (statement)
    (cadr statement)))

(define get-var-expression
  (lambda (statement)
    (if (null? (cddr statement))
        NULL
        (caddr statement))))

(define get-var-value
  (lambda (statement state type)
    (if (null? (cddr statement))
        NULL
        (m-value (get-var-expression statement) state type))))

; return statement handler
(define m-state-return
  (lambda (statement state next break continue return throw type)
    (return state (get-return-value statement state type))))

(define get-return-value
  (lambda (statement state type)
    (m-value (cadr statement) state type)))

; assign statement handler
(define m-state-assign
  (lambda (statement state)
    (if (check-for-binding (get-assign-name statement) state)
        (add-binding (get-assign-name statement) (get-assign-value statement state) (m-state (get-second-operand statement) state))
        (error "Undeclared Variable"))))

(define get-assign-name
  (lambda (statement)
    (cadr statement)))

(define get-assign-value
  (lambda (statement state)
        (m-value (get-second-operand statement) state)))

; function defenition handler
(define m-state-function
  (lambda (statement state next break continue return throw type)
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
     (get-closure-body (get-funcall-closure statement state))
     (bind-parameters-generate-state statement state)
     (lambda (s) (next (recover-state s state (get-funcall-this-name statement))))
     break-error
     continue-error
     (lambda (s v) (next (recover-state s state (get-funcall-this-name statement))))
     (lambda (s v) (throw (recover-state s state (get-funcall-this-name statement)) v)))))

(define (bind-parameters-generate-state statement state type)
  (bind-parameters
   (get-closure-params (get-funcall-closure statement state type))
   (get-funcall-args statement)
   (create-new-binding
    'super
    (update-instance-environment (get-funcall-instance-closure statement state type) (next-scopes (get-instance-environment (get-funcall-instance-closure statement state type))))
    (create-new-binding 'this
                        (if (or (eq? (get-funcall-this-name statement) 'this) (eq? (get-funcall-this-name statement) 'super))
                            (get-binding-value 'this state type)
                            (get-funcall-instance-closure statement state type))
                        (add-environment ((get-closure-environment (get-funcall-closure statement state type)) state))))
   state))

(define get-funcall-closure
  (lambda (statement state type)
    (m-value (cadr statement) state type)))

(define get-funcall-this-name
  (lambda (statement)
    (if (list? (cadadr statement))
         '()
         (cadadr statement))))

(define get-funcall-instance-closure
  (lambda (statement state type)
    (m-value (cadadr statement) state type)))

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
    (cond
      ((xor (null? params) (null? args)) (error "Number of paramaters and arguments in mismatched"))
      ((null? params)                   new-state)
      (else                             (bind-parameters (cdr params) (cdr args) (create-new-binding (car params) (m-value (car args) old-state) new-state) old-state)))))

; class defenition handler
(define m-state-class
  (lambda (statement state next break continue return throw type)
    (next (create-new-binding (get-class-name statement) (make-class-closure (get-class-extends statement) (get-class-body statement) (get-class-name statement)) state))))

(define get-class-name
  (lambda (statement)
    (cadr statement)))

(define get-class-extends
  (lambda (statement)
    (caddr statement)))

(define get-class-body
  (lambda (statement)
    (cadddr statement)))

(define m-state-new
 (lambda (statement state next break continue return throw)
   (next state)))
   
(define m-state-dot
   (lambda (statement state next break continue return throw)
     (next state)))

; === Values expression evaluator ===
(define m-value
  (lambda (expression state type)
    (cond
      ((is-function-expression? expression state) (m-value-function expression state type))
      ((is-object-expression? expression state)   (m-value-object   expression state type))
      ((is-dot-expression? expression state)      (m-value-dot      expression state type))
      ((is-bool-expression? expression state)     (m-bool           expression state type))
      (else                                       (m-number         expression state type)))))

; Check to see if we need to evaluate a function or not
(define is-function-expression?
  (lambda (expression state)
    (and (list? expression) (eq? (get-operator expression) 'funcall))))

; Check to see if we need to evaluate an object or not
(define is-object-expression?
  (lambda (expression state)
    (or
     (eq? expression 'this)
     (eq? expression 'super)
     (and (list? expression) (eq? (get-operator expression) 'new)))))

(define is-dot-expression?
  (lambda (expression state)
    (and (list? expression) (eq? (get-operator expression) 'dot))))

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
  (lambda (expression state type)
    (m-state-body
     (get-closure-body (get-funcall-closure expression state type))
     (bind-parameters-generate-state expression state  type)
     (lambda (s) (error "You can't use the return value from a void function."))
     break-error
     continue-error
     (lambda (s v) v)
     (lambda (s v) (error "How did we get here."))
     type)))

; === Object expression evalutator ===
(define m-value-object
  (lambda (expression state type)
    (cond
      ((single-element? expression)         (get-binding-value expression state type))
      ((eq? (get-operator expression) 'new) (m-value-new expression state type))
      (else                                 (error "Unable to evaluate object")))))

(define m-value-new
  (lambda (expression state type)
   (make-instance-closure (get-first-operand expression) (get-binding-value (get-first-operand expression) state '()) state)))

; === Dot expression evalutator ===
(define m-value-dot
  (lambda (expression state type)
    (get-binding-value (get-second-operand expression) (list (get-instance-environment (m-value (get-first-operand expression) state type))) type)))

; === Numerical expression evaluator ===
(define m-number
  (lambda (expression state type)
    (cond
      ((is-bool-expression? expression state)                       (if (m-bool expression state type) 1 0))  ; Cast bool to number
      ((number? expression)                                         expression)
      ((check-for-binding expression state)                         (get-binding-value expression state type))
      ((single-element? expression)                                 (error "Undeclared Variable"))
      ((contains? (get-operator expression) KEYWORD_MATH_OPERATORS) (m-number-math-operators expression state type))
      ((eq? (get-operator expression) '=)                           (m-number-assign expression state type))
      ((eq? (get-operator expression) 'funcall)                     (m-number (m-value-function expression state type) state type))
      ((eq? (get-operator expression) 'dot)                         (m-number (m-value-dot expression state type) state type))
      (else                                                         (error "This isn't a numerical expression")))))


; === Numerical expression evaluators ===
(define m-number-math-operators
  (lambda (expression state type)
    (cond
      ((eq? (get-operator expression) '+) (m-number-addition       expression state type))
      ((eq? (get-operator expression) '-) (m-number-subtraction    expression state type))
      ((eq? (get-operator expression) '*) (m-number-multiplication expression state type))
      ((eq? (get-operator expression) '/) (m-number-division       expression state type))
      ((eq? (get-operator expression) '%) (m-number-modulus        expression state type)))))

(define (m-number-helper func expression state type)
  (func
   (m-number (get-first-operand expression) state type)
   (m-number (get-second-operand expression) (m-state (get-first-operand expression) state
                                                      identity
                                                      (lambda (s) (error "Called break in expression"))
                                                      (lambda (s) (error "Called continue in expression"))
                                                      (lambda (s) (error "Called return in expression"))
                                                      (lambda (s) (error "Called throw in expression")) type) type)))
   
; addition expression evaluator
(define m-number-addition
  (lambda (expression state type)
    (m-number-helper + expression state type)))
; subtraction expresion evaluator
(define m-number-subtraction
  (lambda (expression state)
    (if (second-operand-exists? expression)
        ; Subtraction
       (m-number-helper - expression state)
        ; Unary
        (- 0 (m-number (get-first-operand expression) state)))))

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
    (not (m-bool (get-first-operand expression) state))))


(define (m-bool-helper func expression state)
  (func
   (m-bool (get-first-operand expression)  state)
   (m-bool (get-second-operand expression) (m-state (get-first-operand expression) state))))

; and expression handler
(define m-bool-and
  (lambda (expression state)
    (m-bool-helper (lambda (a b) (and a b)) expression state)))

; or expression handler
(define m-bool-or
  (lambda (expression state)
    (m-bool-helper (lambda (a b) (or a b)) expression state)))

;(interpret "test-cases/given-tests/part4-test/test07.txt")
