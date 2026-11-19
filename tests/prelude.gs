;; -*- mode: scheme; coding: utf-8; -*-

;;
;; GuileScript's own syntax (e.g. guard) takes precedence over Guile's, which
;; can't be compiled, even if the modules providing it are imported.
;;

(use-modules (ice-9 exceptions) (srfi srfi-9))

(console-log (guard (e (#t (string-append "caught " e))) (raise "x")))
;; => caught x

(define-record-type <pt> (make-pt x) pt? (x pt-x))
(console-log (pt-x (make-pt 7)))
;; => 7
