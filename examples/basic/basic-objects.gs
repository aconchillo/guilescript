;; -*- mode: scheme; coding: utf-8; -*-

;;
;; Arrays and objects.
;;

(define age 34)

;; Vectors (and lists) are arrays.
(define numbers (vector 1 2 age))

;; A quoted association list is an object...
(define person
  '((name . #("Bob" "Smith"))
    (age . 32)))

;; ...and so is js-obj, with values computed when it's created.
(define point (js-obj #:x 1 #:y (+ age 1)))

(console-log (.stringify JSON numbers))
(console-log (.join (.-name person) " "))
(console-log (.stringify JSON point))
;; => [1,2,34]
;; => Bob Smith
;; => {"x":1,"y":35}
