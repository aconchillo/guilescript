;; -*- mode: scheme; coding: utf-8; -*-

;;
;; Guile modules: macros are expanded at compile time, and the procedures
;; that GuileScript compiles inline can be imported (also renamed).
;;

(use-modules (srfi srfi-26)
             (ice-9 match)
             ((srfi srfi-1) #:select ((fold . srfi:fold) iota))
             ((srfi srfi-1) #:prefix s1:)
             ((guile) #:select ((+ . plus) (odd? . odd-number?)))
             (tests guile-modules))

(console-log (map (cut * 2 <>) '(1 2 3)))
;; => [ 2, 4, 6 ]

(console-log (srfi:fold + 0 (iota 4)) (s1:delete-duplicates '(1 1 2)))
;; => 6 [ 1, 2 ]

;; Renamed built-in procedures are compiled inline too, also when they are
;; given to other procedures.
(console-log (plus 1 2) (srfi:fold plus 0 (iota 4)) (filter odd-number? (iota 5)))
;; => 3 6 [ 1, 3 ]

;; Macros from your own Guile modules (tests/guile-modules.scm).
(console-log (twice (+ 1 2)))
;; => [ 3, 3 ]

;; Pattern matching with (ice-9 match).
(define (describe x)
  (match x
    (() "empty")
    ((a) (string-append "one: " (number->string a)))
    ((a b . rest) (string-append "many, starting with " (number->string a)))
    (_ "not a list")))

(console-log (describe '()) (describe '(1)) (describe '(1 2 3)) (describe 42))
;; => empty one: 1 many, starting with 1 not a list

(define (area shape)
  (match shape
    (('circle r) (* 3 r r))
    (('rect w h) (* w h))))

(console-log (area '(circle 2)) (area (list 'rect 2 3)))
;; => 12 6

;; No matching pattern throws an Error.
(js-global process)
(js-invoke process 'on "uncaughtException"
           (lambda (e) (console-log "caught:" (js-ref e 'message))))

(area '(triangle 1 2 3))
;; => caught: match-error match no matching pattern triangle,1,2,3
