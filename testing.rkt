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
(assert "test-cases/given-tests/easy-tests/test15.txt" TRUE)
(assert "test-cases/given-tests/easy-tests/test16.txt" 100)
(assert "test-cases/given-tests/easy-tests/test17.txt" FALSE)
(assert "test-cases/given-tests/easy-tests/test18.txt" TRUE)
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
(assert "test-cases/Levi-Noah-Michael-test/test29.txt" 214216)
; Bool to int casting tests
(assert "test-cases/Levi-Noah-Michael-test/test30.txt" FALSE)
(assert "test-cases/Levi-Noah-Michael-test/test31.txt" TRUE)
(assert "test-cases/Levi-Noah-Michael-test/test32.txt" TRUE)
(assert "test-cases/Levi-Noah-Michael-test/test33.txt" FALSE)


;--------------------
 ; Part2 Tests
 ;--------------------
(assert "test-cases/given-tests/part2-test/test01.txt" 20)
(assert "test-cases/given-tests/part2-test/test02.txt" 164)
(assert "test-cases/given-tests/part2-test/test03.txt" 32)
(assert "test-cases/given-tests/part2-test/test04.txt" 2)
; Error test 5
;(interpret "test-cases/given-tests/part2-test/test05.txt")
;(error "Undeclared Variable")

(assert "test-cases/given-tests/part2-test/test06.txt" 25)
(assert "test-cases/given-tests/part2-test/test07.txt" 21)
(assert "test-cases/given-tests/part2-test/test08.txt" 6)
(assert "test-cases/given-tests/part2-test/test09.txt" -1)
(assert "test-cases/given-tests/part2-test/test10.txt" 789)
; Error test 11, 12, 13
;(interpret "test-cases/given-tests/part2-test/test11.txt")
;(error "Undeclared Variable")
;(interpret "test-cases/given-tests/part2-test/test12.txt")
;(error "Undeclared Variable")
;(interpret "test-cases/given-tests/part2-test/test13.txt")
;(error "Break outside of loop")

(assert "test-cases/given-tests/part2-test/test14.txt" 12)
(assert "test-cases/given-tests/part2-test/test15.txt" 125)
(assert "test-cases/given-tests/part2-test/test16.txt" 110)
(assert "test-cases/given-tests/part2-test/test17.txt" 2000400)
(assert "test-cases/given-tests/part2-test/test18.txt" 101)
; Error test 19
;(interpret "test-cases/given-tests/part2-test/test19.txt" 5)
;(error "Error thrown without catch")
(assert "test-cases/given-tests/part2-test/test20.txt" 21)


;--------------------
 ; Part3 Tests
 ;--------------------
(assert "test-cases/given-tests/part3-test/test01.txt" 10)
(assert "test-cases/given-tests/part3-test/test02.txt" 14)
(assert "test-cases/given-tests/part3-test/test03.txt" 45)
(assert "test-cases/given-tests/part3-test/test04.txt" 55)
(assert "test-cases/given-tests/part3-test/test05.txt" 1)
(assert "test-cases/given-tests/part3-test/test06.txt" 115)
(assert "test-cases/given-tests/part3-test/test07.txt" TRUE)
(assert "test-cases/given-tests/part3-test/test08.txt" 20)
(assert "test-cases/given-tests/part3-test/test09.txt" 24)
(assert "test-cases/given-tests/part3-test/test10.txt" 2)
(assert "test-cases/given-tests/part3-test/test11.txt" 35)
; Error test 12
(interpret "test-cases/given-tests/part3-test/test12.txt")
;(error "TODO")
(assert "test-cases/given-tests/part3-test/test13.txt" 90)
(assert "test-cases/given-tests/part3-test/test14.txt" 69)
(assert "test-cases/given-tests/part3-test/test15.txt" 87)
(assert "test-cases/given-tests/part3-test/test16.txt" 64)
; Error test 17
(interpret "test-cases/given-tests/part3-test/test17.txt")
;(error "TODO")
(assert "test-cases/given-tests/part3-test/test18.txt" 125)
(assert "test-cases/given-tests/part3-test/test19.txt" 100)
(assert "test-cases/given-tests/part3-test/test20.txt" 2000400)
(assert "test-cases/given-tests/part3-test/test21.txt" 3421)
(assert "test-cases/given-tests/part3-test/test22.txt" 20332)
(assert "test-cases/given-tests/part3-test/test23.txt" 21)