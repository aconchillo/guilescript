;; -*- mode: scheme; coding: utf-8; -*-

(define counter 0)

(define (log-counter)
  (console-log counter))

(setInterval log-counter 1000)
