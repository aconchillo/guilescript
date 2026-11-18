;;; (guilescript html) --- GuileScript.

;; Copyright (C) 2026 Aleix Conchillo Flaque <aconchillo@gmail.com>
;;
;; This file is part of GuileScript.
;;
;; GuileScript is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; GuileScript is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GuileScript. If not, see https://www.gnu.org/licenses/.

;;; Commentary:

;; HTML elements with an SXML-like syntax (instead of JSX):
;;
;;   (use-modules (guilescript html))
;;
;;   (html (li #:class-name ,(if done "task done" "task")
;;           (input #:type "checkbox" #:checked done)
;;           (<task-title> #:task task)
;;           "Some text"))
;;
;; html calls h, the hyperscript convention: (h type properties child ...).
;; So it works with any library with such a function:
;;
;;   React:   (js-import "react" #:rename '((createElement . h)))
;;   Preact:  (js-import "preact" h)
;;
;; or with a function that creates DOM elements (see the task manager
;; example).
;;
;; This is a Guile module with a macro, which GuileScript expands at compile
;; time.

;;; Code:

(define-module (guilescript html)
  #:export (html))

;; (html ELEMENT):
;;
;; - (tag property value ... child ...) is an element. tag is an HTML tag
;;   (e.g. li), or a component if it's like <task-title> (the TaskTitle
;;   function in JavaScript).
;;
;; - (<> child ...) is a fragment, like in JSX: it's the same as
;;   (<fragment> child ...), so <fragment> must be defined (e.g. imported
;;   from React with (js-import "react" <fragment>)).
;;
;; - Properties are keywords followed by a value. Keywords become camelCase
;;   names (e.g. #:class-name is className and #:on-click is onClick).
;;
;; - Children and property values are strings, numbers, variables, elements
;;   or ,expression (any expression, e.g. a list of elements). ,@ is the same
;;   as , (lists of elements are valid children).
;;
;; Elements without properties get null as properties.
(define-syntax html
  (lambda (x)
    (syntax-case x ()
      ((html form)
       (let ((h (datum->syntax #'html 'h))
             (js-obj (datum->syntax #'html 'js-obj)))
         (define (component? tag)
           (let ((s (symbol->string (syntax->datum tag))))
             (and (string-prefix? "<" s) (string-suffix? ">" s))))
         (define (value v)
           (syntax-case v (unquote unquote-splicing)
             ((unquote e) #'e)
             ((unquote-splicing e) #'e)
             ((tag . _) (identifier? #'tag) (element v))
             (_ v)))
         (define (element e)
           (syntax-case e ()
             ((tag . rest)
              (identifier? #'tag)
              (let lp ((rest #'rest) (properties '()))
                (syntax-case rest ()
                  ((k v . more)
                   (keyword? (syntax->datum #'k))
                   (lp #'more (append properties (list #'k (value #'v)))))
                  ((child ...)
                   (with-syntax ((type (cond
                                        ((eq? (syntax->datum #'tag) '<>)
                                         (datum->syntax #'tag '<fragment>
                                                        #:source (syntax-source #'tag)))
                                        ((component? #'tag) #'tag)
                                        (else
                                         (datum->syntax #'tag (symbol->string
                                                               (syntax->datum #'tag))))))
                                 ((property ...) properties)
                                 ((child ...) (map value #'(child ...)))
                                 (h h)
                                 (js-obj js-obj))
                     (if (null? properties)
                         #'(h type #nil child ...)
                         #'(h type (js-obj property ...) child ...)))))))
             (_ (syntax-violation 'html "invalid element" e))))
         (element #'form))))))
