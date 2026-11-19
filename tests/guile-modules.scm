;; -*- mode: scheme; coding: utf-8; -*-

;;
;; A Guile module with a macro, used by guile-modules.gs. It's named like
;; that file on purpose: GuileScript must not read it as the file being
;; compiled when Guile compiles it.
;;

(define-module (tests guile-modules)
  #:export (twice))

(define-syntax-rule (twice e)
  (list e e))
