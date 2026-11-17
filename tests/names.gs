;; -*- mode: scheme; coding: utf-8; -*-

;;
;; Scheme names become camelCase JavaScript names.
;;

(define (log-counter n)
  (string-append "counter: " (number->string n)))

(define (all-positive? xs)
  (every positive? xs))

(define *count* 0)

(define (increment-count!)
  (set! *count* (+ *count* 1))
  *count*)

(define (celsius->fahrenheit c)
  (+ 32 (/ (* c 9) 5)))

(define new 'reserved)

(console-log (log-counter 3))
;; => counter: 3

(console-log (all-positive? '(1 2 3)) (all-positive? '(1 -2)))
;; => true false

(increment-count!)
(console-log (increment-count!))
;; => 2

(console-log (celsius->fahrenheit 100))
;; => 212

(console-log new)
;; => reserved

;; Local variables can shadow global ones.
(define x 10)

(define (shadow x)
  (let ((x (* x 2)))
    (+ x 1)))

(console-log (shadow 5) x)
;; => 11 10

(let ((x 1))
  (let ((x (+ x 1)))
    (console-log x)))
;; => 2

;; Module prefixes like lib: separate words too.
(define (lib:greet) "hi")
(console-log (lib:greet))
;; => hi
