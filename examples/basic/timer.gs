;; -*- mode: scheme; coding: utf-8; -*-

;;
;; Count to 3, every 100 milliseconds.
;;
(define counter 0)

(define timer
  (set-interval (lambda ()
                  (set! counter (+ counter 1))
                  (console-log counter)
                  (when (= counter 3)
                    (clear-interval timer)))
                100))
;; => 1
;; => 2
;; => 3
