#lang racket
(require "interpreter.rkt")


;--------------------
; Easy Test Cases
;--------------------
(load "test-cases/given-tests/easy-tests/test1.txt")
(load "test-cases/given-tests/easy-tests/test2.txt")
(load "test-cases/given-tests/easy-tests/test3.txt")
(load "test-cases/given-tests/easy-tests/test4.txt")
(load "test-cases/given-tests/easy-tests/test5.txt")
(load "test-cases/given-tests/easy-tests/test6.txt")
(load "test-cases/given-tests/easy-tests/test7.txt")
(load "test-cases/given-tests/easy-tests/test8.txt")
(load "test-cases/given-tests/easy-tests/test9.txt")
(load "test-cases/given-tests/easy-tests/test10.txt")
(load "test-cases/given-tests/easy-tests/test11.txt")
(load "test-cases/given-tests/easy-tests/test12.txt")
(load "test-cases/given-tests/easy-tests/test13.txt")
(load "test-cases/given-tests/easy-tests/test14.txt")
(load "test-cases/given-tests/easy-tests/test15.txt")
(load "test-cases/given-tests/easy-tests/test16.txt")
(load "test-cases/given-tests/easy-tests/test17.txt")
(load "test-cases/given-tests/easy-tests/test18.txt")
(load "test-cases/given-tests/easy-tests/test19.txt")
(load "test-cases/given-tests/easy-tests/test20.txt")


;--------------------
; Hard Test Cases
;--------------------
(load "test-cases/given-tests/hard-tests/test21.txt")
(load "test-cases/given-tests/hard-tests/test22.txt")
(load "test-cases/given-tests/hard-tests/test23.txt")
(load "test-cases/given-tests/hard-tests/test24.txt")
(load "test-cases/given-tests/hard-tests/test25.txt")
(load "test-cases/given-tests/hard-tests/test26.txt")
(load "test-cases/given-tests/hard-tests/test27.txt")
(load "test-cases/given-tests/hard-tests/test28.txt")


;--------------------
; User-Defined Test Cases
;--------------------
(load "test-cases/our-tests/test29.txt")


(define assert (lambda (filename constraint) (list filename (equal? (interpret filename) constraint))))
;--------------------
; Easy Tests
;--------------------
(assert "test-cases/given-tests/easy-tests/test1.txt" true)
(assert "test-cases/given-tests/easy-tests/test2.txt" true)
(assert "test-cases/given-tests/easy-tests/test3.txt" true)
(assert "test-cases/given-tests/easy-tests/test4.txt" true)
(assert "test-cases/given-tests/easy-tests/test5.txt" true)
(assert "test-cases/given-tests/easy-tests/test6.txt" true)
(assert "test-cases/given-tests/easy-tests/test7.txt" true)
(assert "test-cases/given-tests/easy-tests/test8.txt" true)
(assert "test-cases/given-tests/easy-tests/test9.txt" true)
(assert "test-cases/given-tests/easy-tests/test10.txt" true)
(assert "test-cases/given-tests/easy-tests/test11.txt" true)
(assert "test-cases/given-tests/easy-tests/test12.txt" true)
(assert "test-cases/given-tests/easy-tests/test13.txt" true)
(assert "test-cases/given-tests/easy-tests/test14.txt" true)
(assert "test-cases/given-tests/easy-tests/test15.txt" true)
(assert "test-cases/given-tests/easy-tests/test16.txt" true)
(assert "test-cases/given-tests/easy-tests/test17.txt" true)
(assert "test-cases/given-tests/easy-tests/test18.txt" true)
(assert "test-cases/given-tests/easy-tests/test19.txt" true)
(assert "test-cases/given-tests/easy-tests/test20.txt" true)


 ;--------------------
 ; Hard Tests
 ;--------------------
(assert "test-cases/given-tests/easy-tests/test21.txt" true)
(assert "test-cases/given-tests/easy-tests/test22.txt" true)
(assert "test-cases/given-tests/easy-tests/test23.txt" true)
(assert "test-cases/given-tests/easy-tests/test24.txt" true)
(assert "test-cases/given-tests/easy-tests/test25.txt" true)
(assert "test-cases/given-tests/easy-tests/test26.txt" true)
(assert "test-cases/given-tests/easy-tests/test27.txt" true)
(assert "test-cases/given-tests/easy-tests/test28.txt" true)


 ;--------------------
 ; Our Tests
 ;--------------------
(assert "test-cases/given-tests/easy-tests/test29.txt" true)
