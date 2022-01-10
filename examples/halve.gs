;; -*- mode: scheme; coding: utf-8; -*-

;;
;; Compute the half of a number in a silly way.
;;
(define (halve n)
  (let lp ((x 0) (y 0))
    (if (< y n) (lp (+ x 1) (+ y 2)) x)))
