;; -*- mode: scheme; coding: utf-8; -*-

;;
;; (guilescript html): elements with an SXML-like syntax. html calls h, like
;; hyperscript (e.g. React's createElement or Preact's h).
;;

(use-modules (guilescript html))

;; An h that returns plain objects, to see what html creates.
(define (h type properties . children)
  (js-obj 'type (if (string? type) type (.-name type))
          'properties properties
          'children children))

(define (show element)
  (console-log (.stringify JSON element)))

(show (html (p "Hello")))
;; => {"type":"p","properties":null,"children":["Hello"]}

;; Keywords are camelCase properties, and ,expression is any expression
;; (here, a list of elements).
(define items '("a" "b"))

(show (html (ul #:class-name "items"
                ,(map (lambda (item) (html (li #:key item item))) items))))
;; => {"type":"ul","properties":{"className":"items"},"children":[[{"type":"li","properties":{"key":"a"},"children":["a"]},{"type":"li","properties":{"key":"b"},"children":["b"]}]]}

;; Components are like <name> (TaskItem in JavaScript), and variables can be
;; used directly.
(define (<task-item> properties) #f)
(define done #t)

(show (html (<task-item> #:done done #:count ,(+ 1 2) 42)))
;; => {"type":"TaskItem","properties":{"done":true,"count":3},"children":[42]}

;; Nested elements.
(show (html (div (h1 "Title") (p #:on-click ,(lambda () #t) "text"))))
;; => {"type":"div","properties":null,"children":[{"type":"h1","properties":null,"children":["Title"]},{"type":"p","properties":{},"children":["text"]}]}
