;; -*- mode: scheme; coding: utf-8; -*-

;;
;; (react html): React elements with an SXML-like syntax, instead of JSX.
;;
;; This is a Guile module with a macro, which GuileScript expands at compile
;; time (only macros can be used from Guile modules).
;;

(define-module (react html)
  #:export (html))

;; (html ELEMENT) creates React elements:
;;
;;   (html (li #:class-name ,(if done "task done" "task")
;;           (input #:type "checkbox" #:checked ,done)
;;           (<task-title> #:task task)
;;           "Some text"))
;;
;; - (tag prop value ... child ...) is an element. tag is an HTML tag (e.g.
;;   li), or a component if it's like <task-title> (i.e. TaskTitle).
;;
;; - Properties are keywords followed by a value. Keywords become camelCase
;;   names (#:class-name is className, #:on-click is onClick).
;;
;; - Children and property values are strings, numbers, variables, elements
;;   or ,expression (any Scheme expression, e.g. a list of elements). ,@ is
;;   the same as , (lists of elements are valid children).
;;
;; It uses create-element, imported from React:
;;
;;   (js-import "react" create-element)
(define-syntax html
  (lambda (x)
    (syntax-case x ()
      ((html form)
       (let ((create-element (datum->syntax #'html 'create-element))
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
                   (with-syntax ((type (if (component? #'tag)
                                           #'tag
                                           (datum->syntax #'tag (symbol->string
                                                                 (syntax->datum #'tag)))))
                                 ((property ...) properties)
                                 ((child ...) (map value #'(child ...)))
                                 (create-element create-element)
                                 (js-obj js-obj))
                     (if (null? properties)
                         #'(create-element type #nil child ...)
                         #'(create-element type (js-obj property ...) child ...)))))))
             (_ (syntax-violation 'html "invalid element" e))))
         (element #'form))))))
