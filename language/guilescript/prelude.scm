;;; (language guilescript prelude) --- GuileScript.

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

;; Syntax available in every GuileScript program, in addition to Guile's.

;;; Code:

(define-module (language guilescript prelude)
  #:use-module (srfi srfi-1)
  #:export (guard
            define-record-type))

;; The JavaScript compiler compiles calls to %js-try into:
;;
;;   try { <thunk body> } catch (e) { <handler body> }
;;
;; It also works in Guile.
(define (%js-try thunk handler)
  (with-exception-handler handler thunk #:unwind? #t))

;; R7RS guard: (guard (var clause ...) body ...). The clauses are cond
;; clauses and, if none of them matches, the exception is raised again.
(define-syntax guard
  (syntax-rules ()
    ((_ (var clause ...) body ...)
     (%js-try (lambda () body ...)
              (lambda (var) (%guard-clauses var clause ...))))))

(define-syntax %guard-clauses
  (syntax-rules (else)
    ((_ var clause ... (else e ...))
     (cond clause ... (else e ...)))
    ((_ var clause ...)
     (cond clause ... (else (raise-exception var))))))

;;
;; Records
;;
;; (define-record-type <point> (make-point x y) point? (x point-x set-point-x!))
;;
;; The JavaScript compiler compiles the record type into a class and calls
;; to the record procedures into JavaScript expressions:
;;
;;   (make-point 1 2)    => new Point(1, 2)
;;   (point? obj)        => obj instanceof Point
;;   (point-x p)         => p.x
;;   (set-point-x! p 3)  => p.x = 3
;;
;; The procedures below also work in Guile.

(define (%js-record-type name constructor-fields fields)
  (make-record-type name fields))

(define (%js-record-constructor type constructor-fields)
  (let ((make (record-constructor type))
        (fields (record-type-fields type)))
    (lambda args
      (apply make (map (lambda (field)
                         (let ((i (list-index (lambda (f) (eq? f field)) constructor-fields)))
                           (and i (list-ref args i))))
                       fields)))))

(define (%js-record-predicate type)
  (record-predicate type))

(define (%js-record-accessor type field)
  (record-accessor type field))

(define (%js-record-modifier type field)
  (record-modifier type field))

(define-syntax define-record-type
  (syntax-rules ()
    ((_ type (constructor constructor-field ...) predicate field-spec ...)
     (begin
       (define type
         (%js-record-type 'type '(constructor-field ...) (%record-fields field-spec ...)))
       (define constructor
         (%js-record-constructor type '(constructor-field ...)))
       (define predicate
         (%js-record-predicate type))
       (%define-record-field type field-spec) ...))))

(define-syntax %record-fields
  (syntax-rules ()
    ((_ (field accessor . modifier) ...) '(field ...))))

(define-syntax %define-record-field
  (syntax-rules ()
    ((_ type (field accessor))
     (define accessor (%js-record-accessor type 'field)))
    ((_ type (field accessor modifier))
     (begin
       (define accessor (%js-record-accessor type 'field))
       (define modifier (%js-record-modifier type 'field))))))
