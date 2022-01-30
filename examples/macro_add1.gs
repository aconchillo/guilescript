;; -*- mode: scheme; coding: utf-8; -*-

;;
;; Create a add1 macro that adds 1 to the given number.
;;
(define-syntax add1
  (lambda (x)
    (syntax-case x ()
      ((_ exp)
       (syntax (+ exp 1))))))

(define value 0)

(console-log (add1 value))
