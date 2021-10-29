#lang racket
(require "matrix.rkt")
(require "exponent.rkt")

;;; Title : multiple_regression.rkt
;;; Author : Hiroyuki Shiono
;;; Date : 10/28/2021
;;; Explanation : This code provides the basic implementation of multiple_regression.
;;; You can implement the m x n dimensional array using the structure of (list (list ... ) (list ... ) ...).
;;; as examples below suggested. 
;;; All lists in the list of matrix have must be same lengths and all elements in the matrix must be numbers.

(define mse
  (lambda (coef x y)
    (/ (matrix.calc.mean (matrix.calc.sqr (matrix.calc (matrix.calc.dot coef x) - y))) 2)))

(define gradients
  (lambda (coef x y)
      (list-ref (matrix.transpose (matrix.calc.dot x (matrix.transpose (matrix.calc (matrix.calc.dot coef x) - y)))) 0)))

(define multiple_regression
  (lambda (coef x y learning_rate epsilon)
    (let* ([b1 0.9]
           [b2 0.999]
           [prev_error 0]
           [row (car (matrix.shape coef))]
           [column (cdr (matrix.shape coef))])
      (let* ([m_coef (matrix.zeroes row column)]
             [v_coef (matrix.zeroes row column)]
             [moment_m_coef (matrix.zeroes row column)]
             [moment_v_coef (matrix.zeroes row column)]
             [t 0])
        (display "setup done\n")
        (multiple_regression_helper coef x y learning_rate epsilon
                                    b1 b2 (mse coef x y) prev_error
                                    m_coef v_coef moment_m_coef moment_v_coef 0 0)))))

(define multiple_regression_helper
  (lambda (coef x y learning_rate epsilon
                b1 b2 error prev_error
                m_coef v_coef moment_m_coef moment_v_coef t epochs)
    (if (<= (abs (- error prev_error)) epsilon)
        coef
        (let* ([prev_error error]
               [grad (gradients coef x y)]
               [t (+ t 1)]
               [m_coef (matrix.calc (matrix.calc m_coef * b1) + (list (map (lambda (val)
                                                                       (* (- 1 b1) val))
                                                                       grad)))]
               [v_coef (matrix.calc (matrix.calc v_coef * b2) + (list (map (lambda (val)
                                                                       (* (- 1 b2) val))
                                                                       (map sqr grad))))])
          (let* ([moment_m_coef (matrix.calc m_coef / (- 1 (exponent b1 t)))]
                 [moment_v_coef (matrix.calc v_coef / (- 1 (exponent b2 t)))])
            (let* ([temp1 (matrix.calc (matrix.calc.divisor learning_rate / (matrix.calc.sqrt moment_v_coef))
                                      + 1e-8)]
                   [temp2 (matrix.calc (matrix.calc moment_m_coef * b1) +
                                     (map (lambda (val)
                                            (/ (* (- 1 b1) val) (- 1 (exponent b1 t))))
                                          grad))])
            (let ([delta (matrix.calc temp1 * temp2)])
              (let ([coef (matrix.calc coef - delta)])
                (multiple_regression_helper coef x y learning_rate epsilon
                                            b1 b2 (mse coef x y) prev_error
                                            m_coef v_coef moment_m_coef moment_v_coef t (+ 1 epochs))))))))))

(define predict
  (lambda (new_coef vals)
    (matrix.calc.dot new_coef (matrix.transpose (list vals)))))

;;; example input
#|
(define x (list (list 1 1 1 1 1 1 1 1 1 1)
                (list 38 92 47 29 18 23 24 53 20 31)
                (list 2 4 6 8 10 12 14 16 18 20)
                (list 3 4 7 9 10 13 45 12 29 20)))
(define coef (list (list 1 2 3 4)))
(define y (list (list 280 923 424 258 362 283 124 290 412 203)))
|#
;;; (multiple_regression coef x y 0.05 1e-8)
;;; '((133.59289120109713 7.225829971002383 -2.943005139460761 -1.0716334089295947))
;;; (predict (multiple_regression coef x y 0.05 1e-8) (list 1 38 2 3))
;;; '((399.0735195934774))

