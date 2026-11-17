;; -*- mode: scheme; coding: utf-8; -*-

(define-module (project util)
  #:export (shout))

(define (shout s)
  (string-append (string-upcase s) "!"))
