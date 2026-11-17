;; -*- mode: scheme; coding: utf-8; -*-

;; A GuileScript module using another GuileScript module and a Guile module
;; (for its macros).
(define-module (project greetings)
  #:use-module (project util)
  #:use-module (ice-9 match)
  #:export (greet))

(define (greet name)
  (match name
    ((? string?) (shout (string-append "hello, " name)))
    (_ "?")))
