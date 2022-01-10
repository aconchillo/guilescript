;;; (language guilescript compile) --- GuileScript.

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

;; GuileScript compiler

;;; Code:

(define-module (language guilescript compile)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module ((system base compile) #:prefix system:)
  #:use-module (system base language)
  #:export (compile compile-file))

(define (read-and-parse lang port cenv)
  (let ((exp ((language-reader lang) port cenv)))
    (cond
     ((eof-object? exp) exp)
     ((language-parser lang) => (lambda (parse) (parse exp)))
     (else exp))))

(define (compile port)
  (let* ((from (lookup-language 'scheme))
         (to (lookup-language 'tree-il))
         (joiner (language-joiner to))
         (env (default-environment from)))
    (let lp ((exps '()))
      (match (read-and-parse from port env)
        ((? eof-object?)
         (system:compile (joiner (reverse exps) env) #:from 'guilescript #:to 'ecmascript))
        (e
         (receive (exp)
             (system:compile e #:to 'tree-il)
           (lp (cons exp exps))))))))

(define (compile-file filename)
  (call-with-input-file
      filename
    (lambda (p)
      (let ((js (compile p)))
        (display js)))))
