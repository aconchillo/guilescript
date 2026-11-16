;; -*- mode: scheme; coding: utf-8; -*-

(define age 34)

(define some (vector 1 2 age))

(define person
  '((name . #("Bob" "Smith"))
    (age . 32)))

(console-log (js-ref person 'name))
