;;; (language javascript translate) --- GuileScript.

;; Copyright (C) 2022 Aleix Conchillo Flaque <aconchillo@gmail.com>
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

;; JavaScript function translation

;;; Code:

(define-module (language javascript translate)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (language tree-il)
  #:export (translate-call))

(define (translate-identity proc args recurse port)
  (put-string port (symbol->string proc))
  (put-string port "(")
  (put-string
   port
   (string-join
    (map
     (lambda (arg)
       (call-with-output-string (lambda (p) (recurse arg 'statement 0 p))))
     args)
    ","))
  (put-string port ")"))

;;
;; Operators
;;

(define (binary-operator? op)
  (member op '(< > <= >=)))

(define (arithmetic-operator? op)
  (member op '(+ - * /)))

(define (translate-arithmetic-operator op args recurse port)
  (put-string port "(")
  (put-string
   port
   (string-join
    (map (lambda (arg) (call-with-output-string (lambda (p) (recurse arg 'statement 0 p)))) args)
    (symbol->string op)))
  (put-string port ")"))

(define (translate-binary-operator op args recurse port)
  (put-string port (call-with-output-string (lambda (p) (recurse (first args) 'statement 0 p))))
  (put-string port (symbol->string op))
  (put-string port (call-with-output-string (lambda (p) (recurse (second args) 'statement 0 p)))))

;;
;; Vectors (and strings)
;;

(define (translate-vector-length args recurse port)
  (format port "~a.length" (call-with-output-string (lambda (p) (recurse (first args) 'statement 0 p)))))

(define (translate-vector-ref args recurse port)
  (format port "~a[~a]"
          (call-with-output-string (lambda (p) (recurse (first args) 'statement 0 p)))
          (call-with-output-string (lambda (p) (recurse (second args) 'statement 0 p)))))

(define (translate-vector-set! args recurse port)
  (format port "~a[~a] = ~a"
          (call-with-output-string (lambda (p) (recurse (first args) 'statement 0 p)))
          (call-with-output-string (lambda (p) (recurse (second args) 'statement 0 p)))
          (call-with-output-string (lambda (p) (recurse (third args) 'statement 0 p)))))

;;
;; Objects
;;

(define (translate-js-invoke op args recurse port)
  (let ((obj (call-with-output-string (lambda (p) (recurse (first args) 'statement 0 p))))
        (method (call-with-output-string (lambda (p) (recurse (second args) 'statement 0 p)))))
    (format port "~a.~a" obj method)
    (put-string port "(")
    (put-string
     port
     (string-join
      (map (lambda (arg) (call-with-output-string (lambda (p) (recurse arg 'statement 0 p)))) (cddr args))
      ","))
    (put-string port ")")))

(define (translate-js-new op args recurse port)
  (let ((type (call-with-output-string (lambda (p) (recurse (first args) 'statement 0 p)))))
    (format port "new ~a" type)
    (put-string port "(")
    (put-string
     port
     (string-join
      (map (lambda (arg) (call-with-output-string (lambda (p) (recurse arg 'statement 0 p)))) (cdr args))
      ","))
    (put-string port ")")))

(define (translate-js-ref op args recurse port)
  (let ((obj (call-with-output-string (lambda (p) (recurse (first args) 'statement 0 p))))
        (prop (call-with-output-string (lambda (p) (recurse (second args) 'statement 0 p)))))
    (format port "~a.~a" obj prop)))

(define (translate-js-set! op args recurse port)
  (let ((obj (call-with-output-string (lambda (p) (recurse (first args) 'statement 0 p))))
        (prop (call-with-output-string (lambda (p) (recurse (second args) 'statement 0 p))))
        (value (call-with-output-string (lambda (p) (recurse (third args) 'statement 0 p)))))
    (format port "~a.~a = ~a" obj prop value)))

;; TODO: Probably use a hash table for constant access, but for now this is
;; fine. We could even check for right number of arguments, etc.
(define (translate-call proc args recurse output-name port)
  (let ((op (or (output-name (toplevel-ref-name proc))
                (toplevel-ref-name proc))))
    (match op
      ;; Logging
      ('console-log (translate-identity 'console.log args recurse port))
      ('console-debug (translate-identity 'console.debug args recurse port))
      ('console-error (translate-identity 'console.error args recurse port))
      ('console-warn (translate-identity 'console.warn args recurse port))
      ;; Math
      ('abs (translate-identity 'Math.abs args recurse port))
      ('ceiling (translate-identity 'Math.ceil args recurse port))
      ('floor (translate-identity 'Math.floor args recurse port))
      ('max (translate-identity 'Math.max args recurse port))
      ('min (translate-identity 'Math.min args recurse port))
      ('round (translate-identity 'Math.round args recurse port))
      ;; Strings
      ('string-length (translate-vector-length args recurse port))
      ('string-ref (translate-vector-ref args recurse port))
      ;; Vectors
      ('vector-length (translate-vector-length args recurse port))
      ('vector-ref (translate-vector-ref args recurse port))
      ('vector-set! (translate-vector-set! args recurse port))
      ;; Objects
      ('js-invoke (translate-js-invoke op args recurse port))
      ('js-new (translate-js-new op args recurse port))
      ('js-ref (translate-js-ref op args recurse port))
      ('js-set! (translate-js-set! op args recurse port))
      ;; Operators
      ('equal? (translate-binary-operator '=== args recurse port))
      ((? binary-operator?) (translate-binary-operator op args recurse port))
      ((? arithmetic-operator?) (translate-arithmetic-operator op args recurse port))
      ;; Anything else
      (_ (translate-identity op args recurse port)))))
