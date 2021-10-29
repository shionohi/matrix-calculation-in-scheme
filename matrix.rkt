#lang racket
;;; Title : matrix.rkt
;;; Author : Hiroyuki Shiono
;;; Date : 10/28/2021
;;; Explanation : This code provides the basic matrix operations which is applicable to m x n dimensional array.
;;; You can implement the m x n dimensional array using the structure of (list (list ... ) (list ... ) ...).
;;; as examples below suggested. 
;;; All lists in the list of matrix have must be same lengths and all elements in the matrix must be numbers.

(provide matrix.shape
         matrix.calc
         matrix.calc.divisor
         matrix.calc.dot
         matrix.calc.sqr
         matrix.calc.sqrt
         matrix.calc.mean
         matrix.transpose
         matrix.zeroes)

;;; proc : matrix.shape
;;; input :
;;; m : a matrix, a list of lists
;;; output :
;;; a pair of y and x, where y is the number of m's row (integer)
;;; and x is the number of m's column (integer)

(define matrix.shape
  (lambda (m)
    (cons (length m) (length (car m)))))

;;; proc : nth
;;; input :
;;; m : a matrix, a list of lists
;;; n : an integer, where 0 <= n < (cdr (matrix.shape m))
;;; output :
;;; a list, which contains all elements in m[][n]
;;;
;;; example :
;;; 2 x 2 dimensional matrix
;;;     2 | 4                               
;;; m = - - - -> slice-by-vertical(m, 0) -> (list 2 1)
;;;     1 | 3                               

(define nth
  (lambda (m n)
    (map (lambda (l)
           (list-ref l n))
         m)))

;;; proc : each-line
;;; input :
;;; a : a list of numbers
;;; b : a list of lists
;;; output : a list of lists, initially given as an empty list
;;; index : an integer, 0 <= index < (cdr (matrix.shape m))
;;; 
;;; output :
;;; a list, which contains the dot product of a and b

(define each-line
  (lambda (a b output index)
    (if (equal? (cdr (matrix.shape b)) index)
        output
        (each-line a b (append output (list (apply + (map (lambda (num1 num2)
                                                            (* num1 num2))
                                                          (nth b index)
                                                          a)))) (+ index 1)))))

;;; example : 
;;; (matrix.shape a)
;;; '(2 . 2)
;;; (matrix.shape b)
;;; '(2 . 4)

;;; proc : matrix.calc
;;; purpose : Applies the appropriate matrix calculation based on a given vals
;;; input :
;;; m, a matrix
;;; proc, an expression
;;; vals, either a matrix, a list, or a number
;;; output, a matrix, initially given as empty list
;;; 
;;; output :
;;; a list of lists,  a matrix = m proc vals

(define matrix.calc
  (lambda (m proc vals)
    (matrix.calc.helper m proc vals '())))

;;; example :
;;; > (matrix.calc (list (list 1 2)(list 3 4)) + (list 1 2))
;;; '((2 3) (5 6))
;;; > (matrix.calc (list (list 1 2)(list 3 4)) + 1)
;;; '((2 3) (4 5))
;;; > (matrix.calc (list (list 1 2)(list 3 4)) + (list (list 1 2)(list 3 3.5)))
;;; '((2 4) (6 7.5))

;;; proc : matrix.calc.helper
;;; purpose : conduct the matrix calculation based on the type of vals
;;; input :
;;; m, a matrix
;;; proc, an expression
;;; vals, either a matrix, a list, or a number
;;; output, a matrix, initially given as empty list
;;; 
;;; output :
;;; a list of lists,  a matrix = m proc vals

(define matrix.calc.helper
  (lambda (m proc vals output)
    (cond
      [(equal? (matrix.type? vals) "matrix")
       (matrix.calc.m m proc vals output)]
      [(equal? (matrix.type? vals) "list")
       (matrix.calc.l m proc vals output)]
      [else
       (matrix.calc.n m proc vals output)])))

;;; proc : matrix.type?
;;; purpose : check the type of vals
;;; input :
;;; vals, either matrix, list, or num
;;; output :
;;; str, types of vals

(define matrix.type?
  (lambda (vals)
    (if (list? vals)
        (if (list? (car vals))
            "matrix"
            "list")
        "number")))

;;; proc : matrix.calc.m
;;; input :
;;; m, a matrix
;;; proc, an expression
;;; vals, a matrix
;;; output, a matrix, initially given as empty list
;;; 
;;; output :
;;; a list of lists,  a matrix = m proc vals

(define matrix.calc.m
  (lambda (m proc vals output)
    (if (null? m)
        output
        (matrix.calc.m (cdr m) proc (cdr vals)
                       (append output (list (map (lambda (num val)
                                                   (proc num val))
                                                 (car m)
                                                 (car vals))))))))

;;; proc : matrix.calc.l
;;; input :
;;; m, a matrix
;;; proc, an expression
;;; vals, a list of numbers
;;; output, a matrix, initially given as empty list
;;; 
;;; output :
;;; a list of lists,  a matrix = m proc vals

(define matrix.calc.l
  (lambda (m proc vals output)
    (if (null? m)
        output
        (matrix.calc.l (cdr m) proc (cdr vals)
                       (append output (list (map (lambda (num)
                                                   (proc num (car vals)))
                                                 (car m))))))))

;;; proc : matrix.calc.n
;;; input :
;;; m, a matrix
;;; proc, an expression
;;; vals, a number
;;; output, a matrix, initially given as empty list
;;; 
;;; output :
;;; a list of lists,  a matrix = m proc vals

(define matrix.calc.n
  (lambda (m proc vals output)
    (if (null? m)
        output
        (matrix.calc.n (cdr m) proc vals
                       (append output (list (map (lambda (num)
                                                   (proc num vals))
                                                 (car m))))))))

;;; proc : matrix.calc.divisor-helper
;;; input :
;;; m : a list of lists
;;; val : a number
;;; output : a list of lists, initially given as an empty list
;;; 
;;; output :
;;; a list of lists, where all elements in the output are val / m

(define matrix.calc.divisor-helper
  (lambda (val proc m output)
    (if (null? m)
        output
        (matrix.calc.divisor-helper val proc (cdr m)
                                    (append output
                                            (list (map (lambda (num)
                                                         (proc val num))
                                                       (car m))))))))

;;; proc : matrix.calc.divisor
;;; input :
;;; val : a number
;;; m : a list of lists
;;; proc : an expression
;;; output :
;;; a list of lists, where all elements in the output are val / m

(define matrix.calc.divisor
  (lambda (val proc m)
    (matrix.calc.divisor-helper val proc m '())))

;;; proc : matrix.dot-helper
;;; input :
;;; m1 : a matrix, a list of lists
;;; m2 : a matrix, a list of lists
;;; output : a list of lists, initially defined as an empty list
;;;
;;; output :
;;; a matrix, a list of lists which is a dot product of m1 and m2

(define matrix.calc.dot-helper
  (lambda (m1 m2 output)
    (if (null? m1)
        output
        (matrix.calc.dot-helper (cdr m1) m2 (append output (list (each-line (car m1) m2 '() 0)))))))

;;; proc : matrix.dot
;;; input :
;;; m1 : a matrix, a list of lists
;;; m2 : a matrix, a list of lists
;;; output :
;;; a matrix, a list of lists which is a dot product of m1 and m2

(define matrix.calc.dot
  (lambda (m1 m2)
    (if (not (equal? (cdr (matrix.shape m1))
                     (car (matrix.shape m2))))
        #f
        (matrix.calc.dot-helper m1 m2 '()))))

;;; example : 
;;; (matrix.dot a b)
;;; '((3 13 16 19) (7 29 36 43))

;;; proc : matrix.calc.mean-helper
;;; input :
;;; size : an integer, the size of the m
;;; sum : a number, initially given as a 0
;;; m : a list of lists
;;; output :
;;; a number, the mean of m

(define matrix.calc.mean-helper
  (lambda (size sum m)
    (if (null? m)
        (/ sum size)
        (matrix.calc.mean-helper size (+ sum (apply + (car m))) (cdr m)))))

;;; proc : matrix.calc.mean
;;; input :
;;; m : a list of lists
;;; output :
;;; a number, the mean of m

(define matrix.calc.mean
  (lambda (m)
    (if (empty? m)
        #f
        (matrix.calc.mean-helper (* (car (matrix.shape m))
                                    (cdr (matrix.shape m))) 0 m))))

;;; example :
;;; (matrix.mean a)
;;; 2+1/2
;;; (matrix.mean b)
;;; 4


;;; proc : matrix.calc.sqr-helper
;;; input :
;;; m : a list of lists
;;; val : a number
;;; output : a list of lists, initially given as an empty list
;;; 
;;; output :
;;; a list of lists, where all elements in the output are m - val

(define matrix.calc.sqr-helper
  (lambda (m output)
    (if (null? m)
        output
        (matrix.calc.sqr-helper (cdr m)
                                   (append output
                                           (list(map (lambda (num)
                                                       (* num num))
                                                     (car m))))))))

;;; proc : matrix.calc.sqr
;;; input :
;;; m : a list of lists
;;; val : a number
;;; output :
;;; a list of lists, where all elements in the output are m - val
        
(define matrix.calc.sqr
  (lambda (m)
    (matrix.calc.sqr-helper m '())))

;;; proc : matrix.sqrt-helper
;;; input :
;;; m : a list of lists
;;; val : a number
;;; output : a list of lists, initially given as an empty list
;;; 
;;; output :
;;; a list of lists, where all elements in the output are sqrt(m)

(define matrix.calc.sqrt-helper
  (lambda (m output)
    (if (null? m)
        output
        (matrix.calc.sqrt-helper (cdr m)
                                 (append output
                                         (list(map (lambda (num)
                                                     (sqrt num))
                                                   (car m))))))))

;;; proc : matrix.calc.sqrt
;;; input :
;;; m : a list of lists
;;; val : a number
;;; output :
;;; a list of lists, where all elements in the output are sqrt(m)
        
(define matrix.calc.sqrt
  (lambda (m)
    (matrix.calc.sqrt-helper m '())))

;;; example :
;;; (define a (list (list 1 2)(list 3 4)))

;;; proc : matrix.transpose-helper
;;; input :
;;; m : a list of lists
;;; y : an integer, the size of the column of m
;;; output: a list of lists, intially given as an empty list
;;; index : an integer, 0 <= index < y
;;; 
;;; output :
;;; a list of lists, where all elements in the output tranpose(m)

(define matrix.transpose-helper
  (lambda (m y output index)
    (if (equal? y index)
        output
        (matrix.transpose-helper m y
                                 (append output
                                         (list (nth m index)))
                                 (+ 1 index)))))

;;; proc : matrix.transpose
;;; input :
;;; m : a list of lists
;;; 
;;; output :
;;; a list of lists, where all elements in the output tranpose(m)

(define matrix.transpose
  (lambda (m)
    (matrix.transpose-helper m (cdr (matrix.shape m)) '() 0)))

;;; example
;;; (matrix.transpose a)
;;; '((1 3) (2 4))
;;; (matrix.transpose b)
;;; '((1 1) (3 5) (4 6) (5 7))

;;; proc : matrix.zeroes-helper
;;; input :
;;; x : an integer
;;; y : an integer
;;; output : a list of lists, initially given as an empty list
;;; 
;;; output :
;;; a list of lists, x by y dimensional matrix where all values are zeroes.

(define matrix.zeroes-helper
  (lambda (x y output)
    (if (equal? x 0)
        output
        (matrix.zeroes-helper (- x 1) y (append output (list (make-list y 0)))))))

;;; proc : matrix.zeroes
;;; input :
;;; x : an integer
;;; y : an integer
;;; output :
;;; a list of lists, x by y dimensional matrix where all values are zeroes.

(define matrix.zeroes
  (lambda (x y)
    (matrix.zeroes-helper x y '())))

;;; example : 
;;; (matrix.zeroes 3 0)
;;; '(() () ())
;;; (matrix.zeroes 7 2)
;;; '((0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0))