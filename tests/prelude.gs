;; -*- mode: scheme; coding: utf-8; -*-

;;
;; GuileScript's own syntax (e.g. guard) takes precedence over Guile's, which
;; can't be compiled, even if the modules providing it are imported.
;;

(use-modules (ice-9 exceptions))

(console-log (guard (e (#t (string-append "caught " e))) (raise "x")))
;; => caught x
