;; -*- mode: scheme; coding: utf-8; -*-

;;
;; Variables that are never used are not declared (their value is still
;; evaluated if it has side effects).
;;

(define (f)
  (define a 1)
  (console-log "between definitions")
  (define b (+ a 1))
  (let ((unused (console-log "evaluated")))
    b))

(console-log (f))
;; => between definitions
;; => evaluated
;; => 2
