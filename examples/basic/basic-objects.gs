;; -*- mode: scheme; coding: utf-8; -*-

;;
;; Arrays and objects.
;;

(define age 34)

;; Vectors (and lists) are arrays.
(define numbers (vector 1 2 age))

;; Objects are created with js-obj.
(define person
  (js-obj #:name #("Bob" "Smith")
          #:age age))

(define point (js-obj #:x 1 #:y (+ age 1)))

(console-log (.stringify JSON numbers))
(console-log (.join (.-name person) " ") (.-age person))
(console-log (.stringify JSON point))
;; => [1,2,34]
;; => Bob Smith 34
;; => {"x":1,"y":35}
