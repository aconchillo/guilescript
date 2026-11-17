;; -*- mode: scheme; coding: utf-8; -*-

;;
;; A tiny helper to create DOM elements.
;;

(define-module (dom)
  #:export (h set-children!))

(js-global document)

;; (h "li" (js-obj #:class-name "task") child ...) creates an element with
;; the given properties and children. Children are strings, elements or lists
;; of children, and false children are ignored.
(define (h tag properties . children)
  (let ((element (.create-element document tag)))
    (.assign Object element properties)
    (for-each (lambda (child) (append-child! element child)) children)
    element))

;; Replace the children of ELEMENT (children are like in h).
(define (set-children! element . children)
  (.replace-children element)
  (for-each (lambda (child) (append-child! element child)) children))

(define (append-child! element child)
  (if (list? child)
      (for-each (lambda (c) (append-child! element c)) child)
      (when child
        (.append element child))))
