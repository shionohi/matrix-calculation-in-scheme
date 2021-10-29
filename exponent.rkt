#lang racket
;;; title : exponent.rkt
;;; explanation : provide an exponent method using divide and conquer approach
;;; author : Hiroyuki Shiono
;;; date : Oct 29 2021

(provide exponent)

;;; proc : exponent
;;; input :
;;; a, a number
;;; b, an integer
;;; output :
;;; a number, a^b

(define exponent
  (lambda (a b)
    (if (equal? b 0)
        1
        (if (even? b)
            (* (exponent a (/ b 2))(exponent a (/ b 2)))
            (* a (exponent a (- b 1)))))))

;;; example : 
;;; input : (exponent 2 4)
;;; output : 16
;;; input : (exponent 3 3)
;;; output : 27a