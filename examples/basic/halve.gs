;; -*- mode: scheme; coding: utf-8; -*-

;;
;; Compute the half of a number in a silly way (a named let is a loop).
;;
(define (halve n)
  (let lp ((x 0) (y 0))
    (if (< y n) (lp (+ x 1) (+ y 2)) x)))

(console-log (halve 42))
;; => 21
