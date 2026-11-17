;; -*- mode: scheme; coding: utf-8; -*-

(define (classify n)
  (cond ((< n 0) 'negative)
        ((= n 0) 'zero)
        ((< n 10) 'small)
        (else 'large)))

(console-log (map classify '(-5 0 5 50)))
;; => [ 'negative', 'zero', 'small', 'large' ]

(define (kind x)
  (case x
    ((1 2 3) 'low)
    ((a) 'letter)
    (else 'other)))

(console-log (kind 2) (kind 'a) (kind 9))
;; => low letter other

;; and/or follow JavaScript truthiness.
(define (first-truthy a b c) (or a b c))
(console-log (first-truthy #f 0 "x") (first-truthy #f #f #f))
;; => x false

(define (both a b) (and a b))
(console-log (both 1 2) (both #f 2))
;; => 2 false

(define (sign-label n)
  (unless (= n 0)
    (if (> n 0) "positive" "negative")))

(console-log (sign-label 5) (sign-label 0))
;; => positive undefined

(define (lookup alist key)
  (cond ((assoc-ref alist key) => (lambda (v) (* v 10)))
        (else 0)))

(console-log (lookup '((a . 1) (b . 2)) 'b) (lookup '((a . 1)) 'z))
;; => 20 0

(define (describe n)
  (let ((parity (if (even? n) "even" "odd"))
        (size (cond ((> n 100) "big") (else "small"))))
    (string-append parity " " size)))

(console-log (describe 7) (describe 200))
;; => odd small even big

(define (safe-div a b)
  (when (= b 0)
    (error "division by zero:" a))
  (/ a b))

(console-log (safe-div 10 2))
;; => 5

;; Errors are thrown as JavaScript Error objects.
(js-global process)
(js-invoke process 'on "uncaughtException"
           (lambda (e) (console-log "caught:" (js-ref e 'message))))

(safe-div 1 0)
;; => caught: division by zero: 1
