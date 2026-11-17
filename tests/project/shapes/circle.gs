;; -*- mode: scheme; coding: utf-8; -*-

(define-module (project shapes circle)
  #:use-module ((project util) #:prefix u:)
  #:export (<circle> make-circle circle? circle-area circle-label))

(define-record-type <circle>
  (make-circle radius)
  circle?
  (radius circle-radius))

(define (circle-area c)
  (* 3 (circle-radius c) (circle-radius c)))

(define (circle-label c)
  (u:shout (string-append "circle " (number->string (circle-radius c)))))
