#lang racket
(require "classParser.rkt")



(define KEYWORD_MATH_OPERATORS '(+ - * / %))
(define KEYWORD_BOOL_OPERATORS '(&& || !))
(define KEYWORD_COMPARATORS    '(== != < > <= >=))
(define KEYWORD_CONTROL        '(var = return if while begin try catch finally break continue throw function static-function funcall class new dot))
(define EMPTY_STATE            '((() ())))
(define EMPTY_ENVIRONMENT      '(() ()))
(define NULL 'null)
(define TRUE 'true)
(provide TRUE)
(define FALSE 'false)
(provide FALSE)

(define MAIN 'main)

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

(define get-scope-names
  (lambda (scope)
    (car scope)))

(define get-scope-values
  (lambda (scope)
    (cadr scope)))

(define make-scope
  (lambda (names values)
    (list names values)))

(define get-next-scope-elements
  (lambda (scope)
    (make-scope (cdr (get-scope-names scope)) (cdr (get-scope-values scope)))))

; Check whether a name is bound
(define check-for-binding
  (lambda (name state type)
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
  (lambda (name state type)
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

; replace the last environments of outer states with the environments of inner state
(define recover-state
  (lambda (inner-state outer-state instance-name type)
    (recover-this instance-name inner-state (recover-state-cps (get-next-environments inner-state)
                       outer-state
                       (- (get-environment-count outer-state) (get-environment-count (get-next-environments  inner-state))) identity)
                  type)))

(define recover-state-cps
  (lambda (inner-state outer-state skip-count return)
    (cond
      ((null? inner-state) (return '()))
      ((> skip-count 0)    (recover-state-cps inner-state (cdr outer-state) (- skip-count 1) (lambda (s) (return (cons (car outer-state) s)))))
      (else                (return inner-state)))))

(define recover-this
  (lambda (instance-name inner-state merged-state type)
    (if (null? instance-name)
        merged-state
        (update-binding instance-name (get-binding-value 'this inner-state type) merged-state))))

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
  (lambda (instance-type class-closure state type)
    (list instance-type (make-instance-closure-environment class-closure state type))))

(define make-instance-closure-environment
  (lambda (class-closure state type)
    (if (null? (get-class-super-type class-closure))
        (get-class-scope class-closure)
        (append (get-class-scope class-closure) (make-instance-closure-environment (get-binding-value (get-class-super-type class-closure) state type) state  type)))))

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
  (lambda (statement state next break continue return throw type)
    (if (second-operand-exists? statement)
        (m-state-operators-two statement state next break continue return throw type)
        (m-state-operators-one statement state next break continue return throw type))))

(define m-state-operators-one
  (lambda (statement state next break continue return throw type)
    (m-state (get-first-operand statement) state next break continue return throw type)))

(define m-state-operators-two
  (lambda (statement state next break continue return throw type)
    (m-state (get-first-operand statement) state (lambda (s) (m-state (get-second-operand statement) s next break continue return throw type))
             break continue return throw type)))

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

; m-state-body-begin
(define (m-state-body-begin statementlist state next break continue return throw type)
  (if (null? statementlist)
      (next state)
      (m-state-body statementlist (add-scope state)
                    (lambda (s) (next (remove-scope s)))
                    (lambda (s) (break (remove-scope s)))
                    (lambda (s) (continue (remove-scope s)))
                    (lambda (s v) (return (remove-scope s) v))
                    (lambda (s v) (throw (remove-scope s) v))
                    type)))

(define (one-statement? statementlist)
  (or (not (list? (car statementlist))) (null? (cdr statementlist))))

(define (m-state-body statementlist state next break continue return throw type)
  (if (one-statement? statementlist)
      (if (not (list? (car statementlist)))
          (m-state statementlist state next break continue return throw type)
          (m-state  (car statementlist) state next break continue return throw type))
      (m-state (car statementlist) state (lambda (s) (m-state-body (cdr statementlist) s next break continue return throw type))
               break continue return throw type)))


; begin/ {} statement handler
(define (m-state-begin statement state next break continue return throw type)
  (m-state-body-begin (get-statement-list statement) state next break continue return throw type))

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
  (lambda (statement state next break continue return throw type)
    (m-state-try (get-try-block-statement-list statement) state
                 (lambda (s) (m-state-finally (get-finally-block-statement-list statement) s next break continue return throw type)) ; new next
                 (lambda (s) (m-state-finally (get-finally-block-statement-list statement) s break break continue return throw type)); new break
                 (lambda (s) (m-state-finally (get-finally-block-statement-list statement) s continue break continue return throw type)); new continue
                 (lambda (s1 v) (m-state-finally (get-finally-block-statement-list statement) s1 (lambda (s2) (return s2 v)) break continue return throw type))
                 (lambda (s1 v) (m-state-catch (get-catch-block-statement-list statement) (create-new-binding (get-catch-exception-name statement) v s1)
                                               (lambda (s2) (m-state-finally (get-finally-block-statement-list statement) s2 next break continue return throw type))
                                               (lambda (s2) (m-state-finally (get-finally-block-statement-list statement) s2 break break continue return throw type))
                                               (lambda (s2) (m-state-finally (get-finally-block-statement-list statement) s2 continue break continue return throw type))
                                               (lambda (s2 v2) (m-state-finally (get-finally-block-statement-list statement) s2 (lambda (s3) (return s3 v2)) break continue return throw type))
                                               (lambda (s2 v2) (m-state-finally (get-finally-block-statement-list statement) s2 (lambda (s3) (throw s3 v2)) break continue return throw type)) type)) type)))
     
; try block handler
(define m-state-try
  (lambda (statementlist state next break continue return throw type)
    (m-state-body-begin statementlist state next break continue return throw type)))

; catch block handler
(define m-state-catch
  (lambda (statementlist state next break continue return throw type)
    (m-state-body-begin statementlist state next break continue return throw type)))

; finally block handler
(define m-state-finally
  (lambda (statementlist state next break continue return throw type)
    (m-state-body-begin statementlist state next break continue return throw type)))

; continue
(define m-state-continue
  (lambda (statementlist state next break continue return throw type)
    (continue state)))

; break
(define m-state-break
  (lambda (statementlist state next break continue return throw type)
    (break state)))

; throw
(define get-throw-value
  (lambda (statement state type)
    (m-value (cadr statement) state type)))

(define m-state-throw
  (lambda (statementlist state next break continue return throw type)
    (throw state (get-throw-value statementlist state type))))

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

(define (m-state-if statement state next break continue return throw type)
  (if (m-bool (get-condition statement) state type)
      (m-state (get-condition statement) state (lambda (s) (m-state-body-begin (get-then statement) s next break continue return throw type))
               break continue return throw type)
      
      (if (else-exist? statement)
          (m-state (get-condition statement) state (lambda (s) (m-state-body-begin (get-else statement) s next break continue return throw type))
                   break continue return throw type)
          (m-state (get-condition statement) state next break continue return throw type))))

; while statement handler
(define (m-state-while statement state next break continue return throw type)
  (if (m-bool (get-condition statement) state type)
      (m-state (get-condition statement) state (lambda (s1)
                                                 (m-state-body-begin (get-loop-body statement) s1 (lambda (s2)
                                                                                                    (m-state-while statement s2 next break continue return throw type))
                                                                     next (lambda (s3)
                                                                            (m-state-while statement s3 next break continue return throw type )) return throw type))
               break continue return throw type)
      (m-state (get-condition statement) state next break continue return throw type)))

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
  (lambda (statement state next break continue return throw type)
    (if (list? (get-assign-name statement)) (m-state-assign-this statement state next break continue return throw type)
        (if (check-for-binding (get-assign-name statement) state type)
            (m-state (get-second-operand statement) state (lambda (s) (next (update-binding (get-assign-name statement) (get-assign-value statement state type) s))) break continue return throw type)
            (error "Undeclared Variable")))))

(define m-state-assign-this
  (lambda (statement state next break continue return throw type)
    (cond
      ((not (eq? (get-operator (get-assign-name statement)) 'dot))       (error "Tried to assign to an expression"))
      ((not (eq? (get-first-operand (get-assign-name statement)) 'this)) (error "Tried to write to private instance variable"))
      (else
       (m-state (get-second-operand statement) state
                (lambda (s) (next (update-binding
                                   'this
                                   (update-instance-environment
                                    (get-binding-value 'this state type)
                                    (update-binding-cps-scope (get-second-operand (get-assign-name statement))
                                                              (get-assign-value statement state type)
                                                              (get-instance-environment (get-binding-value 'this state type))
                                                              identity))
                                   state)))
                break continue return throw type)))))

(define get-assign-name
  (lambda (statement)
    (cadr statement)))

(define get-assign-value
  (lambda (statement state type)
    (m-value (get-second-operand statement) state type)))

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
  (lambda (statement state next break continue return throw type)
    (m-state-body
     (get-closure-body (get-funcall-closure statement state type))
     (bind-parameters-generate-state statement state type)
     (lambda (s) (next (recover-state s state (get-funcall-this-name statement) type)))
     break-error
     continue-error
     (lambda (s v) (next (recover-state s state (get-funcall-this-name statement) type)))
     (lambda (s v) (throw (recover-state s state (get-funcall-this-name statement) type) v))
     type)))

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
   state type))

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
  (lambda (params args new-state old-state type)
    (cond
      ((xor (null? params) (null? args)) (error "Number of paramaters and arguments in mismatched"))
      ((null? params)                   new-state)
      (else                             (bind-parameters (cdr params) (cdr args) (create-new-binding (car params) (m-value (car args) old-state type) new-state) old-state type)))))

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
 (lambda (statement state next break continue return throw type)
   (next state)))
   
(define m-state-dot
   (lambda (statement state next break continue return throw type)
     (next state)))

; === Values expression evaluator ===
(define m-value
  (lambda (expression state type)
    (cond
      ;((eq? expression 'super)                    (m-value-super expression state))
      ((is-function-expression? expression state)  (m-value-function expression state type))
      ((is-object-expression? expression state)    (m-value-object   expression state type))
      ((is-dot-expression? expression state)       (m-value-dot      expression state type))
      ((is-bool-expression? expression state type) (m-bool           expression state type))
      (else                                        (m-number         expression state type)))))

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
  (lambda (expression state type)
    (cond
      ((eq? expression 'true)                                       #t)
      ((eq? expression 'false)                                      #t)
      ((number? expression)                                         #f)
      ((check-for-binding expression state type)                    (boolean? (get-binding-value expression state type)))
      ((single-element? expression)                                 #f)
      ((contains? (get-operator expression) KEYWORD_BOOL_OPERATORS) #t)
      ((contains? (get-operator expression) KEYWORD_COMPARATORS)    #t)
      (else                                                         #f))))

; === Function expression evaluator ===
(define m-value-function
  (lambda (expression state type)
    (m-state-body
     (get-closure-body (get-funcall-closure expression state type))
     (bind-parameters-generate-state expression state type)
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
   (make-instance-closure (get-first-operand expression) (get-binding-value (get-first-operand expression) state type) state type)))

; === Dot expression evalutator ===
(define m-value-dot
  (lambda (expression state type)
    (get-binding-value (get-second-operand expression) (list (get-instance-environment (m-value (get-first-operand expression) state type))) type)))

; === Numerical expression evaluator ===
(define m-number
  (lambda (expression state type)
    (cond
      ((is-bool-expression? expression state type)                  (if (m-bool expression state type) 1 0))  ; Cast bool to number
      ((number? expression)                                         expression)
      ((check-for-binding expression state type)                    (get-binding-value expression state type))
      ((single-element? expression)                                 (display type) (error "Undeclared Variable"))
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
                                                      (lambda (s) (error "Called throw in expression"))
                                                      type) type)))
   
; addition expression evaluator
(define m-number-addition
  (lambda (expression state type)
    (m-number-helper + expression state type)))
; subtraction expresion evaluator
(define m-number-subtraction
  (lambda (expression state type)
    (if (second-operand-exists? expression)
        ; Subtraction
        (m-number-helper - expression state type)
        ; Unary
        (- 0 (m-value (get-first-operand expression) state type)))))

; multiplication expresion evaluatotar
(define m-number-multiplication
  (lambda (expression state type)
    (m-number-helper * expression state type)))

; division expresion evaluator
(define m-number-division
  (lambda (expression state type)
    (m-number-helper quotient expression state type)))

; modulus expresion evaluator
(define m-number-modulus
  (lambda (expression state type)
    (m-number-helper modulo expression state type)))


; assignment expression evaluator
(define m-number-assign
  (lambda (expression state type)
    (get-assign-value expression state type)))

; === Boolean expression evaluator ===
(define m-bool
  (lambda (expression state type)
    (cond
      ((equal? expression #t)                                       #t)
      ((equal? expression #f)                                       #f)
      ((equal? expression 'true)                                    #t)
      ((equal? expression 'false)                                   #f)
      ((check-for-binding expression state type)                    (get-binding-value expression state type))
      ((single-element? expression)                                 (error "Undeclared Variable"))
      ((contains? (get-operator expression) KEYWORD_BOOL_OPERATORS) (m-bool-bool-operators expression state type))
      ((contains? (get-operator expression) KEYWORD_COMPARATORS)    (m-bool-comparators    expression state type))
      ((eq? (get-operator expression) 'funcall)                     (m-bool (m-value-function expression state type) state type))
      ((eq? (get-operator expression) 'dot)                         (m-bool (m-value-dot expression state) state type) type)
      (else                                                         (error "This isn't a boolean expression")))))

; === Comparison operator expression evaluator ===
(define m-bool-comparators
  (lambda (expression state type)
    (cond
      ((eq? (get-operator expression) '==) (m-bool-equals              expression state type))
      ((eq? (get-operator expression) '!=) (m-bool-not-equals          expression state type))
      ((eq? (get-operator expression) '<)  (m-bool-less-than           expression state type))
      ((eq? (get-operator expression) '>)  (m-bool-greater-than        expression state type))
      ((eq? (get-operator expression) '<=) (m-bool-less-than-equals    expression state type))
      ((eq? (get-operator expression) '>=) (m-bool-greater-than-equals expression state type)))))

; equals expression evaluator
(define m-bool-equals
  (lambda (expression state type)
    (m-number-helper equal? expression state type)))

; not equals expression evaluator
(define m-bool-not-equals
  (lambda (expression state type)
    (not (m-number-helper equal? expression state type))))


; less than expression evaluator
(define m-bool-less-than
  (lambda (expression state type)
    (m-number-helper < expression state type)))


; greater than expression evaluator
(define m-bool-greater-than
  (lambda (expression state type)
    (m-number-helper > expression state type)))


; less than or equal to expression evaluator
(define m-bool-less-than-equals
  (lambda (expression state type)
    (m-number-helper <= expression state type)))
       

; greater than or equal to expression evaluator
(define m-bool-greater-than-equals
  (lambda (expression state type)
    (m-number-helper >= expression state type)))

; === Boolean operator expression handlers ===
(define m-bool-bool-operators
  (lambda (expression state type)
    (cond
      ((eq? (get-operator expression) '!)  (m-bool-not expression state type))
      ((eq? (get-operator expression) '&&) (m-bool-and expression state type))
      ((eq? (get-operator expression) '||) (m-bool-or  expression state type)))))

; not expression handler
(define m-bool-not
  (lambda (expression state type)
    (not (m-value (get-first-operand expression) state type))))


(define (m-bool-helper func expression state type)
  (func
   (m-bool (get-first-operand expression) state type)
   (m-bool (get-second-operand expression) (m-state (get-first-operand expression) state
                                                    identity
                                                    (lambda (s) (error "Called break in expression"))
                                                    (lambda (s) (error "Called continue in expression"))
                                                    (lambda (s) (error "Called return in expression"))
                                                    (lambda (s) (error "Called throw in expression")) type) type)))

; and expression handler
(define m-bool-and
  (lambda (expression state type)
    (m-bool-helper (lambda (a b) (and a b)) expression state type)))

; or expression handler
(define m-bool-or
  (lambda (expression state type)
    (m-bool-helper (lambda (a b) (or a b)) expression state type)))

(interpret "test-cases/given-tests/part4-test/test07.txt")