#lang racket
(require rackunit)
(require rackunit/text-ui)
(require rackunit/gui)
(require "examples.rkt")

(define lecture2-tests
  (test-suite
   "Lecture 2 Tests"
   (test-case
    "length"
    (check-equal? (length '())
                  0
                  "(length '())")
    (check-equal? (length '(1 2))
                  2
                  "(length '(1 2))")
    (check-equal? (length '(1 (2 3)))
                  2
                  "(length '(1 (2 3)))"))

   (test-case
    "sum"
    (check-equal? (sum '(1 2 3 4))
                  10
                  "(sum '(1 2 3 4))"))

   (test-case
    "nth"
    (check-equal? (nth 0 '(1 2 3))
                  1
                  "(nth 0 '(1 2 3))")
    (check-equal? (nth 2 '(1 2 3))
                  3
                  "(nth 2 '(1 2 3))"))

   (test-case
    "concat"
    (check-equal? (concat '(1 2 3) '(4 5 6))
                  '(1 2 3 4 5 6)
                  "(concat '(1 2 3) '(4 5 6))"))

   (test-case
    "numints"
    (check-equal? (numints 5)
                  1
                  "(numints 5)")
    (check-equal? (numints '(1 2 (3 4) 5))
                  5
                  "(numints '(1 2 (3 4) 5))"))

   (test-case
    "map"
    (check-equal? (map (lambda (x) (+ x 1)) '(1 2 3))
                  '(2 3 4)
                  "(map (lambda (x) (+ x 1)) '(1 2 3))"))

   (test-case
    "sum-of-squares"
    (check-equal? (sum-of-squares '(1 2 3 4 5))
                  55
                  "(sum-of-squares '(1 2 3 4 5))"))

   (test-case
    "reduce"
    (check-equal? (reduce + 0 '(1 2 3 4))
                  10
                  "(reduce + 0 '(1 2 3 4))")
    (check-equal? (reduce * 1 '(1 2 3 4))
                  24
                  "(reduce * 1 '(1 2 3 4))"))

   (test-case
    "order"
    (check-equal? (order '(1 (2 (3)) 4))
                  3
                  "(order '(1 (2 (3)) 4))")
    (check-equal? (order '((())))
                  2
                  "(order '((())))"))))

(test/gui lecture2-tests)

;(when (not (eq? (run-tests lecture2-tests) 0))
;    (exit 1))
