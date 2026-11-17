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
  #:export (guard))

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
