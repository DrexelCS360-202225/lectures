#lang racket
(require racket/trace)

(define (enumerate-from n)
  (cons n (enumerate-from (+ n 1))))

(define (stream-enumerate-from n)
  (stream-append (stream n) (stream-enumerate-from (+ n 1))))
