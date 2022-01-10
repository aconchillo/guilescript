;;; (language guilescript spec) --- GuileScript.

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

;; GuileScript language definition

;;; Code:

(define-module (language guilescript spec)
  #:use-module (language guilescript compile-ecmascript)
  #:use-module (system base compile)
  #:use-module (system base language)
  #:export (guilescript))

;;;
;;; Language definition
;;;

(define-language guilescript
  #:title	"GuileScript"
  #:reader      (lambda (port env)
                  ;; Use the binding of current-reader from the environment.
                  ;; FIXME: Handle `read-options' as well?
                  ((or (and=> (and=> (module-variable env 'current-reader)
                                     variable-ref)
                              fluid-ref)
                       read-syntax)
                   port))
  #:compilers   `((ecmascript . ,compile-ecmascript))
  ;; a pretty-printer would be interesting.
  #:printer	write)
