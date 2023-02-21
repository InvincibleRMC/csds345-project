#lang racket
(require "interpreter.rkt")

(define assert (lambda (filename constraint)
                 (display (string-append (substring filename 34 38) " " (substring filename 38 39)
                                         (if (number? (string->number (substring filename 39 40)))
                                             (substring filename 39 40)
                                             "")))
                          (equal? (interpret filename) constraint)))
;--------------------
; Easy Tests
;--------------------
(assert "test-cases/given-tests/easy-tests/test1.txt" 150)
(assert "test-cases/given-tests/easy-tests/test2.txt" -4)
(assert "test-cases/given-tests/easy-tests/test3.txt" 10)
(assert "test-cases/given-tests/easy-tests/test4.txt" 16)
(assert "test-cases/given-tests/easy-tests/test5.txt" 220)
(assert "test-cases/given-tests/easy-tests/test6.txt" 5)
(assert "test-cases/given-tests/easy-tests/test7.txt" 6)
(assert "test-cases/given-tests/easy-tests/test8.txt" 10)
(assert "test-cases/given-tests/easy-tests/test9.txt" 5)
(assert "test-cases/given-tests/easy-tests/test10.txt" -39)

; Error Tests
#|
(interpret "test-cases/given-tests/easy-tests/test11.txt")
(error "Undeclared Variable")

(interpret "test-cases/given-tests/easy-tests/test12.txt")
(error "Undeclared Variable")

(interpret "test-cases/given-tests/easy-tests/test13.txt")
(error "Tried to evaluate expression with an uninitialized variable")

(interpret "test-cases/given-tests/easy-tests/test14.txt")
(error "Variable Already Declared")
|#
(assert "test-cases/given-tests/easy-tests/test15.txt" true)
(assert "test-cases/given-tests/easy-tests/test16.txt" 100)
(assert "test-cases/given-tests/easy-tests/test17.txt" false)
(assert "test-cases/given-tests/easy-tests/test18.txt" true)
(assert "test-cases/given-tests/easy-tests/test19.txt" 128)
(assert "test-cases/given-tests/easy-tests/test20.txt" 12)


 ;--------------------
 ; Hard Tests
 ;--------------------
(assert "test-cases/given-tests/hard-tests/test21.txt" 30)
(assert "test-cases/given-tests/hard-tests/test22.txt" 11)
(assert "test-cases/given-tests/hard-tests/test23.txt" 1106)
(assert "test-cases/given-tests/hard-tests/test24.txt" 12)
(assert "test-cases/given-tests/hard-tests/test25.txt" 16)
(assert "test-cases/given-tests/hard-tests/test26.txt" 72)
(assert "test-cases/given-tests/hard-tests/test27.txt" 21)
(assert "test-cases/given-tests/hard-tests/test28.txt" 164)


 ;--------------------
 ; Our Tests
 ;--------------------
(assert "test-cases/our-tests/test29.txt" true)
; Bool to int casting tests
(assert "test-cases/our-tests/test30.txt" false)
(assert "test-cases/our-tests/test31.txt" true)
(assert "test-cases/our-tests/test32.txt" true)
(assert "test-cases/our-tests/test33.txt" false)
