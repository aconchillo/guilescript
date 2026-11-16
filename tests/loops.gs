;; -*- mode: scheme; coding: utf-8; -*-

;;
;; Named let loops compile to while loops, so they don't grow the stack.
;;

(define (count-up n)
  (let loop ((i 0) (acc '()))
    (if (< i n)
        (loop (+ i 1) (cons i acc))
        (reverse acc))))

(console-log (count-up 5))
;; => [ 0, 1, 2, 3, 4 ]

(define (sum-to n)
  (let loop ((i 0) (total 0))
    (if (> i n)
        total
        (loop (+ i 1) (+ total i)))))

(console-log (sum-to 100000))
;; => 5000050000

;; Functions that call themselves in tail position too.
(define (countdown n)
  (if (= n 0)
      'done
      (countdown (- n 1))))

(console-log (countdown 1000000))
;; => done

;; Loop variables are updated in parallel.
(define (fib n)
  (let loop ((a 0) (b 1) (i 0))
    (if (= i n)
        a
        (loop b (+ a b) (+ i 1)))))

(console-log (fib 50))
;; => 12586269025

(do ((i 0 (+ i 1)))
    ((= i 3))
  (console-log "do" i))
;; => do 0
;; => do 1
;; => do 2

;; Nested loops
(define (pairs n)
  (let outer ((i 0) (acc '()))
    (if (= i n)
        (reverse acc)
        (let inner ((j 0) (acc acc))
          (if (= j n)
              (outer (+ i 1) acc)
              (inner (+ j 1) (cons (list i j) acc)))))))

(console-log (length (pairs 3)) (car (pairs 3)) (last (pairs 3)))
;; => 9 [ 0, 0 ] [ 2, 2 ]

;; Closures capture the value of each iteration.
(define thunks
  (let loop ((i 0) (acc '()))
    (if (= i 3)
        (reverse acc)
        (loop (+ i 1) (cons (lambda () i) acc)))))

(console-log (map (lambda (f) (f)) thunks))
;; => [ 0, 1, 2 ]

;; A loop used as a value.
(console-log (+ 1 (let loop ((i 0)) (if (< i 10) (loop (+ i 1)) i))))
;; => 11

(define (print-evens n)
  (let loop ((i 0))
    (when (< i n)
      (when (even? i)
        (console-log i))
      (loop (+ i 1)))))

(print-evens 5)
;; => 0
;; => 2
;; => 4

;; Internal helper functions with tail calls are loops too.
(define (digits n)
  (define (go n acc)
    (if (< n 10)
        (cons n acc)
        (go (quotient n 10) (cons (remainder n 10) acc))))
  (go n '()))

(console-log (digits 1234))
;; => [ 1, 2, 3, 4 ]

;; Same for functions calling themselves.
(define (collect n acc)
  (if (= n 0)
      acc
      (collect (- n 1) (cons (lambda () n) acc))))

(console-log (map (lambda (f) (f)) (collect 3 '())))
;; => [ 1, 2, 3 ]
