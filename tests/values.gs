;; -*- mode: scheme; coding: utf-8; -*-

;;
;; Multiple values are arrays (a single value is just the value). receive,
;; let-values and friends are compiled into array destructuring.
;;

(use-modules (ice-9 receive) (srfi srfi-11))

(define (div-mod a b)
  (values (quotient a b) (remainder a b)))

(receive (q r) (div-mod 17 5)
  (console-log q r))
;; => 3 2

(call-with-values (lambda () (div-mod 7 2))
  (lambda (q r) (console-log q r)))
;; => 3 1

;; A single value is just the value.
(console-log (values 42) (call-with-values (lambda () 5) (lambda (x) (* x 2))))
;; => 42 10

;; Rest values.
(receive (first . rest) (values 1 2 3)
  (console-log first rest))
;; => 1 [ 2, 3 ]

(let-values (((q r) (div-mod 9 4))
             ((s) (values 10)))
  (console-log q r s))
;; => 2 1 10

(let*-values (((a b) (values 1 2))
              ((c) (values (+ a b))))
  (console-log c))
;; => 3

;; As a value.
(console-log (+ 100 (receive (q r) (div-mod 9 4) (+ q r))))
;; => 103

;; If the consumer isn't a lambda, the values are its arguments.
(console-log (call-with-values (lambda () (values 1 2 3)) +))
;; => 6

;; Tail calls inside receive are still loops.
(define (sum-div-mods pairs)
  (let loop ((pairs pairs) (total 0))
    (if (null? pairs)
        total
        (receive (q r) (div-mod (car (car pairs)) (cadr (car pairs)))
          (loop (cdr pairs) (+ total q r))))))

(console-log (sum-div-mods '((7 2) (9 4))))
;; => 7

;; define-values
(define-values (x y) (values 1 2))
(console-log x y)
;; => 1 2

(define (div-mod-sum a b)
  (define-values (q r) (div-mod a b))
  (+ q r))

(console-log (div-mod-sum 10 3))
;; => 4

(define-values (first-value . other-values) (values 1 2 3))
(console-log first-value other-values)
;; => 1 [ 2, 3 ]

(define-values (only) (values 'one))
(console-log only)
;; => one

;; A single value is never destructured, even if it's a list or a string.
(define one '(5))
(call-with-values (lambda () "abc") console-log)
;; => abc
(console-log (call-with-values (lambda () (car one)) (lambda args args))
             (call-with-values (lambda () (values '(1 2 3))) (lambda (a . rest) (list a rest))))
;; => [ 5 ] [ [ 1, 2, 3 ], [] ]
