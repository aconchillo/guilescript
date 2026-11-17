;; -*- mode: scheme; coding: utf-8; -*-

;;
;; Constants, numbers, booleans and strings.
;;

(console-log 42 -7 0.5 1/4)
;; => 42 -7 0.5 0.25

(console-log "a\"b\\c")
;; => a"b\c

(console-log "line 1\nline 2")
;; => line 1
;; => line 2

(console-log #t #f #nil)
;; => true false null

;; Like in Scheme, round rounds to even.
(console-log (round 2.5) (round 3.5) (round -2.5) (round 1.4) (map round '(0.5 1.5)))
;; => 2 4 -2 1 [ 0, 2 ]

;; Integer division.
(define-values (q r) (floor/ -7 2))
(console-log q r (truncate/ -7 2) (modulo -7 2) (remainder -7 2) (quotient -7 2))
;; => -4 1 [ -3, -1 ] 1 -1 -3

;; string->number is #f for anything that is not a number (including "").
(define empty "")
(console-log (string->number "-12.5") (string->number "1e3") (string->number empty)
             (string->number " 42") (string->number "0x10") (string->number "abc"))
;; => -12.5 1000 false false false false

(console-log (string->number "ff" 16) (string->number "fg" 16) (string->number "101" 2))
;; => 255 false 5

;; Symbols and characters are strings.
(console-log 'hello-world #\x)
;; => hello-world x

(console-log (+ 1 2 3) (- 10 4) (- 5) (* 2 3 4) (/ 10 4) (/ 2) (+))
;; => 6 6 -5 24 2.5 0.5 0

(console-log (< 1 2) (< 1 3 2) (= 2 2) (<= 1 1 2) (> 3 2 1))
;; => true false true true true

(console-log (quotient 7 2) (remainder -7 2) (modulo -7 2) (expt 2 10) (expt -2 2))
;; => 3 -1 1 1024 4

(console-log (abs -3) (max 1 5 3) (min 4 2) (floor 2.7) (round 2.4) (sqrt 16))
;; => 3 5 2 2 2 4

(console-log (not #f) (zero? 0) (even? 4) (odd? 4) (1+ 1) (1- 1))
;; => true true true false 2 0

(console-log (number->string 255 16) (string->number "42") (number? 1) (integer? 1.5))
;; => ff 42 true false

;; GuileScript uses JavaScript truthiness: 0 and "" are false.
(define (truthy x) (if x 'yes 'no))
(console-log (truthy 0) (truthy "") (truthy '()) (truthy #f) (truthy 1))
;; => no no yes no yes
