;;; (language js-tree-il spec) --- GuileScript.

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

;; JavaScript Tree-IL language definition

;; Certain things are not possible with Guile's Tree-IL. For example, support
;; for multiple values using receive/values. It would be great if GuileScript
;; had its own JavaScript Tree-IL such that we could have 100% control of how
;; GuileScript code is converted to JavaScript. For now, we create a dummy
;; js-tree-il language in the hope that some day this exists.

;;; Code:

(define-module (language js-tree-il spec)
  #:use-module (ice-9 match)
  #:use-module (language tree-il)
  #:use-module (system base language)
  #:export (js-tree-il))

(define (join exps env)
  (match exps
    (() (make-void #f))
    ((x) x)
    ((x . rest)
     (make-seq #f x (join rest env)))
    (_ (error "what!" exps env))))

;;;
;;; Language definition
;;;

(define-language js-tree-il
  #:title	"JavaScript Tree Intermediate Language"
  #:reader	(lambda (port env) (read port))
  #:printer	write
  #:joiner      join
  #:for-humans? #f)
