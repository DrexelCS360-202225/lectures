#lang racket
(require racket/trace)
(require "tree.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Higher Order Function Examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; Define a function map that takes a function f and a list xs and returns a new
; list consisting of the results of applying f to each element in xs.
;
; Example:
;  (map (lambda (x) (+ x 1)) '(1 2 3)) => '(2 3 4)
;

(define (map f xs)
  (if (null? xs)
      null
      (cons (f (first xs)) (map f (rest xs)))))

;
; Compute the sum of a list of integers
;
; Example:
;   (sum '(1 2 3 4)) => 10

(define (sum xs)
  (if (null? xs)
      0
      (+ (first xs) (sum (rest xs)))))

;
; Calculate the squares of a list of integers. Make the function non-recursive.
;
; Example:
;   (squares '(1 2 3 4 5)) => '(1 4 9 16 25)
;

(define (squares xs)
  (map (lambda (x) (* x x)) xs))

;
; Calculate the sum of squares of a list of integers. Make the function non-recursive.
;
; Example:
;   (sum-of-squares '(1 2 3 4 5)) => 55
;

(define (sum-of-squares xs)
  (sum (squares xs)))

;
; Return a list containing only the elements of xs for which the predicate f
; returns true.
;
; Examples:
;   (filter (lambda (x) (> x 0)) '(-1 -4 5 6 0)) => '(5 6)
;

(define (filter f xs)
  (cond [(null? xs)     null]
        [(f (first xs)) (cons (first xs) (filter f (rest xs)))]
        [else           (filter f (rest xs))]))
      

;
; Reduce a list of values to a single value by repeatedly application of a binary function. 
;
; Examples:
;   (reduce + 0 '(1 2 3 4)) => 10
;   (reduce * 1 '(1 2 3 4)) => 24
;

(define (reduce f z xs)
  (if (null? xs)
      z
      (f (car xs) (reduce f z (rest xs)))))
  

;
; Write a non-recursive function that takes a list and returns all the even
; integers in the list
;
; Examples:
;   (only-even '(1 2 3 4 5)) => '(2 4)

(define (only-even xs)
  'not-implemented)

; Write a function that composes two functions.
; That is, ((compose f g) x) should be the same as (f (g x))

(define (compose f g)
  (lambda (x) (f (g x))))

; Write a function that partially applies a function to a single argument.
; That is, ((papply f x) y) should be the same as (f x y)

(define (papply f x)
  (lambda (y) (f x y)))

; Add one to a number
; Define using papply

(define inc
  (papply + 1))

;;
;; Folds
;;

; Right fold

(define (foldr f z xs)
  (if (null? xs)
      z
      (f (first xs) (foldr f z (rest xs)))))

;; (show-tree (foldr (show-function 'cons) 'null '(1 2 3 4)))

; Left fold

(define (foldl f z xs)
  (if (null? xs)
      z
      (foldl f (f z (first xs)) (rest xs))))

;; (show-tree (foldl (show-function 'cons) 'null '(1 2 3 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State Examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)
