;; -*- mode: scheme; coding: utf-8; -*-

;;
;; Compute a Fibonacci number.
;;
(define (fib n)
  (if (<= n 1)
      1
      (+ (fib (- n 2)) (fib (- n 1)))))

(console-log (fib 10))
;; => 89
