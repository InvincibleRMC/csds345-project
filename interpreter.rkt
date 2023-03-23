#lang racket
(require "simpleParser.rkt")

(define KEYWORD_MATH_OPERATORS '(+ - * / %))
(define KEYWORD_BOOL_OPERATORS '(&& || !))
(define KEYWORD_COMPARATORS    '(== != < > <= >=))
(define KEYWORD_CONTROL        '(var = return if while begin try catch finally))
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
    (m-state-body (parser filename) EMPTY_STATE  identity identity identity identity identity)))

(define interpreter-helper
  (lambda (statementlist state  next break continue return throw)
    ; (if (check-for-binding RETURN state)
    ; (interpret-return-output (get-binding-value RETURN state))
    (interpreter-helper (cdr statementlist) (m-state (car statementlist) state  next break continue return throw)  next break continue return throw)))

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
  (lambda (name state)
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

;(state-length '(() ()))
;(state-length '((x) (3)))
;(state-length '((x c) (3 2)))

; Add a name-value pair binding to the state, or replce the value if name is already bound
(define add-binding
  (lambda (name value state)
    (add-binding-cps name value state (lambda (v) v))))

(define add-binding-cps
  (lambda (name value state return)
    (cond
      ((and (not (is-last-state? state))
            (not (contains? name (get-state-names (get-current-state state))))) (add-binding-cps name value (next-state state)
                                                                                                 (lambda (v) (return (append (get-current-state state) v)))))
      (else (add-one-state-binding-cps name value (get-current-state state)
                                       (lambda (v) (return (append v (next-state state)))))))))
      
(define (add-one-state-binding-cps name value state return)
  (cond
    ((and (= 0 (state-length state))
          (is-last-state? state))         (return (make-state (list name) (list value))))
    ((eq? (car (get-state-names (get-current-state state))) name) (return (append (make-state (get-state-names (get-current-state state))
                                                                                              (cons value (cdr (get-state-values (get-current-state state))))) (next-state state))))
    
      
    (else (add-one-state-binding-cps name value (append (make-state (cdr (get-state-names (get-current-state state)))
                                                                    (cdr (get-state-values (get-current-state state)))) (next-state state))
                                     (lambda (v) (return (make-state
                                                          (cons (car (get-state-names  (get-current-state state))) (get-state-names  (get-current-state v)))
                                                          (cons (car (get-state-values (get-current-state state))) (get-state-values (get-current-state v))))))))))

; Get the keyword the defines the statement type from a statement represented by a list
(define get-statement-type
  (lambda (statement)
    (car statement)))


; === State handler ===
; Modify the state by a statement
(define m-state
  (lambda (statement state next break continue return throw)
    (cond
      ((null? statement)                                                 state)
      ((eq? statement NULL)                                              state)
      ((single-element? statement)                                       state)
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
    (m-state (get-second-operand statement) (m-state (get-first-operand statement) state next break continue return throw)
             next break continue return throw)))

; === Control flow state handlers ===
(define m-state-control
  (lambda (statement state next break continue return throw)
    (cond
      ((eq? (get-statement-type statement) 'var)     (m-state-var               statement state next break continue return throw))
      ((eq? (get-statement-type statement) 'return)  (m-state-return            statement state next break continue return throw))
      ((eq? (get-statement-type statement) '=)       (m-state-assign            statement state next break continue return throw))
      ((eq? (get-statement-type statement) 'if)      (m-state-if                statement state next break continue return throw))
      ((eq? (get-statement-type statement) 'while)   (m-state-while             statement state next break continue return throw))
      ((eq? (get-statement-type statement) 'begin)   (m-state-begin             statement state next break continue return throw))
      ((eq? (get-statement-type statement) 'try)     (m-state-try-catch-finally statement state next break continue return throw))
      ;((eq? (get-statement-type statement) 'catch)   (m-state-catch   statement state next break continue return throw))
      ;((eq? (get-statement-type statement) 'finally) (m-state-finally statement state next break continue return throw))
      (else                                         (error "Unknown Control Keyword")))))



; m-state-body

; m-state-body-begin
(define (m-state-body-begin statementlist state next break continue return throw)
  (m-state-body statementlist (add-scope state) next break continue return throw))

(define (one-statement? statementlist)
  (or (not (list? (car statementlist))) (null? (cdr statementlist))))

(define (m-state-body statementlist state next break continue return throw)
  (if (one-statement? statementlist)
      (if (not (list? (car statementlist)))
          (m-state statementlist state next break continue return throw)
          (m-state  (car statementlist) state next break continue return throw))
      (m-state-body (cdr statementlist) (m-state (car statementlist) state next break continue return throw)
                    next break continue return throw)))


; begin/ {} statement handler
(define (m-state-begin statement state next break continue return throw)
  (m-state-body-begin (get-statement-list statement) state next break continue return throw))
       


(define (get-statement-list statement)
  (cdr statement))


;'((var x) (try ((= x 20) (if (< x 10) (throw 10)) (= x (+ x 5))) (catch (e) ((= x e))) (finally ((= x (+ x 100))))) (return x))
; try statement handler
(define catch-exist?
  (lambda (statement)
    (eq? 'catch (caaddr statement))))

(define finally-exist?
  (lambda (statement)
    (if (null? (cdddr statement))
        (eq? 'finally (caaddr statement))
        (eq? 'finally (car (cadddr statement))))))

(define get-try-block-statement-list
  (lambda (statement)
    (cadr statement)))

(define get-catch-exception-name
  (lambda (statement)
    (caadr (caddr statement))))

(define m-state-try-catch-finally
  (lambda (statement state next break continue return throw)
    (cond
      ((and (catch-exist? statement)
            (finally-exist? statement))  (m-state-try ;both finally and catch
                                          (get-try-block-statement-list statement)
                                          state ; doesn't change here
                                          (m-state-finally state next break continue return throw); new next
                                          (m-state-finally state break break continue return throw); new break
                                          (m-state-finally state continue break continue return throw); newcontinue
                                          (m-state-finally state return break continue return throw); new return
                                          (m-state-catch                                               ; this throw
                                           (add-binding (get-catch-exception-name statement) NULL state)
                                           (m-state-finally state next break continue return throw); new next
                                           (m-state-finally state break break continue return throw); new break
                                           (m-state-finally state continue break continue return throw); newcontinue
                                           (m-state-finally state return break continue return throw); new return
                                           (m-state-finally state throw break continue return throw)))); new throw
      ((catch-exist? statement)          (m-state-try ;only catch
                                          (get-try-block-statement-list statement)
                                          state next break continue return
                                          (m-state-catch
                                           (add-binding (get-catch-exception-name statement) NULL state)
                                           next break continue return throw)))
      ((finally-exist? statement)        (m-state-try ;only finally 
                                          (get-try-block-statement-list statement)
                                          (m-state-finally state next break continue return throw); new next
                                          (m-state-finally state break break continue return throw); new break
                                          (m-state-finally state continue break continue return throw); newcontinue
                                          (m-state-finally state return break continue return throw); new return
                                          throw))
      (else                              (error "Malformed try statement")))))

;;try block handler
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
  (if (m-bool (get-condition statement) state next break continue return throw)
      (m-state-body-begin (get-then statement) (m-state (get-condition statement) state next break continue return throw)
                          next break continue return throw)
      
      (if (else-exist? statement)
          (m-state-body-begin (get-else statement) (m-state (get-condition statement)  state next break continue return throw) next break continue return throw)
          (m-state (get-condition statement) state next break continue return throw))))

; while statement handler
(define (m-state-while statement state next break continue return throw)
  (if (m-bool (get-condition statement) state next break continue return throw)
      (m-state-while statement(m-state-body-begin (get-loop-body statement) (m-state (get-condition statement) state next break continue return throw)
                                                  next break continue return throw)
                     next break continue return throw)
      (m-state (get-condition statement) state next break continue return throw)))

(define get-loop-body
  (lambda (statement)
    (caddr statement)))


; var statement handler
(define m-state-var
  (lambda (statement state next break continue return throw)
    (if (check-for-binding (get-var-name statement) state)
        (error "Variable Already Declared")
        (add-binding (get-var-name statement) (get-var-value statement state next break continue return throw)(m-state (get-var-expression statement) state next break continue return throw)))))

(define get-var-name
  (lambda (statement)
    (cadr statement)))

(define get-var-expression
  (lambda (statement)
    (if (null? (cddr statement))
        NULL
        (caddr statement))))

(define get-var-value
  (lambda (statement state next break continue return throw)
    (if (null? (cddr statement))
        NULL
        (m-value (get-var-expression statement) state next break continue return throw))))

; return statement handler
(define m-state-return
  (lambda (statement state next break continue return throw)
    (return (get-return-value statement state next break continue return throw))))

(define get-return-value
  (lambda (statement state next break continue return throw)
    (m-value (cadr statement) state next break continue return throw)))

; assign statement handler
(define m-state-assign
  (lambda (statement state next break continue return throw)
    (if (check-for-binding (get-assign-name statement) state)
        (add-binding (get-assign-name statement) (get-assign-value statement state  next break continue return throw) (m-state (get-second-operand statement) state  next break continue return throw))
        (error "Undeclared Variable"))))

(define get-assign-name
  (lambda (statement)
    (cadr statement)))

(define get-assign-value
  (lambda (statement state  next break continue return throw)
    (m-value (get-second-operand statement) state  next break continue return throw)))

; === Values expression evaluator
(define m-value
  (lambda (expression state next break continue return throw)
    (if (is-bool-expression? expression state)
        (m-bool   expression state next break continue return throw)
        (m-number expression state next break continue return throw))))

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

; === Numerical expression evaluator ===
(define m-number
  (lambda (expression state next break continue return throw)
    (cond
      ((is-bool-expression? expression state)                       (if (m-bool expression state next break continue return throw) 1 0))  ; Cast bool to number
      ((number? expression)                                         expression)
      ((check-for-binding expression state)                         (get-binding-value expression state))
      ((single-element? expression)                                 (error "Undeclared Variable"))
      ((contains? (get-operator expression) KEYWORD_MATH_OPERATORS) (m-number-math-operators expression state next break continue return throw))
      ((eq? (get-operator expression) '=)                           (m-number-assign expression state next break continue return throw))
      (else                                                         (error "This isn't a numerical expression")))))


; === Numerical expression evaluators ===
(define m-number-math-operators
  (lambda (expression state next break continue return throw)
    (cond
      ((eq? (get-operator expression) '+) (m-number-addition       expression state next break continue return throw))
      ((eq? (get-operator expression) '-) (m-number-subtraction    expression state next break continue return throw))
      ((eq? (get-operator expression) '*) (m-number-multiplication expression state next break continue return throw))
      ((eq? (get-operator expression) '/) (m-number-division       expression state next break continue return throw))
      ((eq? (get-operator expression) '%) (m-number-modulus        expression state next break continue return throw)))))

(define (m-number-helper func expression state next break continue return throw)
  (func
   (m-number (get-first-operand expression) state next break continue return throw)
   (m-number (get-second-operand expression) (m-state (get-first-operand expression) state next break continue return throw)
             next break continue return throw)))
   
; addition expression evaluator
(define m-number-addition
  (lambda (expression state next break continue return throw)
    (m-number-helper + expression state next break continue return throw)))
; subtraction expresion evaluator
(define m-number-subtraction
  (lambda (expression state next break continue return throw)
    (if (second-operand-exists? expression)
        ; Subtraction
        (m-number-helper - expression state next break continue return throw)
        ; Unary
        (- 0 (m-number (get-first-operand expression) state next break continue return throw)))))

; multiplication expresion evaluator
(define m-number-multiplication
  (lambda (expression state next break continue return throw)
    (m-number-helper * expression state next break continue return throw)))

; division expresion evaluator
(define m-number-division
  (lambda (expression state next break continue return throw)
    (m-number-helper quotient expression state next break continue return throw)))

; modulus expresion evaluator
(define m-number-modulus
  (lambda (expression state next break continue return throw)
    (m-number-helper modulo expression state next break continue return throw)))


; assignment expression evaluator
(define m-number-assign
  (lambda (expression state next break continue return throw)
    (get-assign-value expression state next break continue return throw)))

; === Boolean expression evaluator ===
(define m-bool
  (lambda (expression state next break continue return throw)
    (cond
      ((equal? expression 'true)                                    #t)
      ((equal? expression 'false)                                   #f)
      ((check-for-binding expression state)                         (get-binding-value      expression state))
      ((single-element? expression)                                 (error "Undeclared Variable"))
      ((contains? (get-operator expression) KEYWORD_BOOL_OPERATORS) (m-bool-bool-operators expression state next break continue return throw))
      ((contains? (get-operator expression) KEYWORD_COMPARATORS)    (m-bool-comparators    expression state next break continue return throw))
      (else                                                         (error "This isn't a boolean expression")))))

; === Comparison operator expression evaluator ===
(define m-bool-comparators
  (lambda (expression state next break continue return throw)
    (cond
      ((eq? (get-operator expression) '==) (m-bool-equals              expression state next break continue return throw))
      ((eq? (get-operator expression) '!=) (m-bool-not-equals          expression state next break continue return throw))
      ((eq? (get-operator expression) '<)  (m-bool-less-than           expression state next break continue return throw))
      ((eq? (get-operator expression) '>)  (m-bool-greater-than        expression state next break continue return throw))
      ((eq? (get-operator expression) '<=) (m-bool-less-than-equals    expression state next break continue return throw))
      ((eq? (get-operator expression) '>=) (m-bool-greater-than-equals expression state next break continue return throw)))))

; equals expression evaluator
(define m-bool-equals
  (lambda (expression state next break continue return throw)
    (m-number-helper equal? expression state next break continue return throw)))

; not equals expression evaluator
(define m-bool-not-equals
  (lambda (expression state next break continue return throw)
    (not (m-number-helper equal? expression state next break continue return throw))))


; less than expression evaluator
(define m-bool-less-than
  (lambda (expression state next break continue return throw)
    (m-number-helper < expression state next break continue return throw)))


; greater than expression evaluator
(define m-bool-greater-than
  (lambda (expression state next break continue return throw)
    (m-number-helper > expression state next break continue return throw)))


; less than or equal to expression evaluator
(define m-bool-less-than-equals
  (lambda (expression state next break continue return throw)
    (m-number-helper <= expression state next break continue return throw)))
       

; greater than or equal to expression evaluator
(define m-bool-greater-than-equals
  (lambda (expression state next break continue return throw)
    (m-number-helper >= expression state next break continue return throw)))

; === Boolean operator expression handlers ===
(define m-bool-bool-operators
  (lambda (expression state next break continue return throw)
    (cond
      ((eq? (get-operator expression) '!)  (m-bool-not expression state next break continue return throw))
      ((eq? (get-operator expression) '&&) (m-bool-and expression state next break continue return throw))
      ((eq? (get-operator expression) '||) (m-bool-or  expression state next break continue return throw)))))

; not expression handler
(define m-bool-not
  (lambda (expression state next break continue return throw)
    (not (m-bool (get-first-operand expression) state next break continue return throw))))


(define (m-bool-helper func expression state next break continue return throw)
  (func
   (m-bool (get-first-operand expression)  state next break continue return throw)
   (m-bool (get-second-operand expression) (m-state (get-first-operand expression) state next break continue return throw)
           next break continue return throw)))

; and expression handler
(define m-bool-and
  (lambda (expression state next break continue return throw)
    (m-bool-helper (lambda (a b) (and a b)) expression state next break continue return throw)))

; or expression handler
(define m-bool-or
  (lambda (expression state next break continue return throw)
    (m-bool-helper (lambda (a b) (or a b)) expression state next break continue return throw)))


;(parser "test-cases/given-tests/easy-tests/test16.txt")
;(interpret "test-cases/given-tests/easy-tests/test16.txt")
;(interpret "test-cases/given-tests/easy-tests/test15.txt")