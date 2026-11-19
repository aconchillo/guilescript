;; -*- mode: scheme; coding: utf-8; -*-

;; Closures
(define (make-counter)
  (let ((n 0))
    (lambda ()
      (set! n (+ n 1))
      n)))

(define counter (make-counter))
(counter)
(console-log (counter))
;; => 2

;; Internal definitions
(define (hypot a b)
  (define (square x) (* x x))
  (define sum (+ (square a) (square b)))
  (sqrt sum))

(console-log (hypot 3 4))
;; => 5

;; Mutually recursive internal definitions
(define (parity n)
  (define (ev? n) (if (= n 0) #t (od? (- n 1))))
  (define (od? n) (if (= n 0) #f (ev? (- n 1))))
  (if (ev? n) 'even 'odd))

(console-log (parity 7))
;; => odd

;; Optional, rest and keyword arguments
(define* (greet name #:optional (greeting "Hello"))
  (string-append greeting ", " name))

(console-log (greet "Ann") (greet "Bob" "Hi"))
;; => Hello, Ann Hi, Bob

(define (sum . xs)
  (apply + xs))

(console-log (sum) (sum 1 2 3))
;; => 0 6

(define (count-rest a b . rest)
  (length rest))

(console-log (count-rest 1 2 3 4))
;; => 2

(define* (make-point #:key (x 0) (y 0))
  (list x y))

(console-log (make-point #:y 5) (make-point))
;; => [ 0, 5 ] [ 0, 0 ]

;; case-lambda
(define area
  (case-lambda
    ((r) (* 3 r r))
    ((w h) (* w h))))

(console-log (area 2) (area 2 3))
;; => 12 6

;; Higher-order functions and apply
(define (compose f g)
  (lambda (x) (f (g x))))

(console-log ((compose (lambda (x) (* x 2)) 1+) 5))
;; => 12

(console-log (apply max 1 '(5 3)) (apply + 1 2 '(3 4)) (apply list '(1 2)))
;; => 5 10 [ 1, 2 ]

;; Built-in procedures used as values take all their arguments.
(define nums '(1 2 3))
(define (call2 f) (f 6 2))
(console-log (apply - nums) (apply < nums) (apply < '(1 2 0)) (apply substring (list "hello" 1 3)))
;; => -4 true false el
(console-log (call2 -) (call2 /) (call2 >) (call2 number->string) (call2 iota))
;; => 4 3 true 110 [ 2, 3, 4, 5, 6, 7 ]
(define rows '((1 2) (3 4)))
(console-log (apply map list rows) (map (lambda (f) (f 10 2)) (list - = max)))
;; => [ [ 1, 3 ], [ 2, 4 ] ] [ 8, false, 10 ]

(define add (lambda (a b) (+ a b)))
(console-log (add 1 2) (map car '((1 2) (3 4))))
;; => 3 [ 1, 3 ]
