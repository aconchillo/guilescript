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
  #:use-module (language scheme compile-tree-il)
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
  #:compilers   `((js-tree-il . ,compile-tree-il))
  #:evaluator	(lambda (x module) (primitive-eval x))
  #:printer	write
  #:make-default-environment
                (lambda ()
                  ;; Ideally we'd duplicate the whole module hierarchy so that `set!',
                  ;; `fluid-set!', etc. don't have any effect in the current environment.
                  (let ((m (make-fresh-user-module)))
                    ;; Provide a separate `current-reader' fluid so that
                    ;; compile-time changes to `current-reader' are
                    ;; limited to the current compilation unit.
                    (module-define! m 'current-reader (make-fluid))

                    ;; Default to `simple-format', as is the case until
                    ;; (ice-9 format) is loaded.  This allows
                    ;; compile-time warnings to be emitted when using
                    ;; unsupported options.
                    (module-set! m 'format simple-format)

                    m)))
