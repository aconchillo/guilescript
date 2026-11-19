;; -*- mode: scheme; coding: utf-8; -*-

;;
;; GuileScript modules (tests/project/...) are compiled into ES modules.
;;

(use-modules (project greetings)
             ((project shapes circle) #:select (make-circle circle-label (circle-area . area)))
             ((project util) #:prefix u:))

(console-log (greet "Ann") (greet 42))
;; => HELLO, ANN! ?

(define c (make-circle 2))
(console-log (area c) (circle-label c))
;; => 12 CIRCLE 2!

(console-log (u:shout "done"))
;; => DONE!
