;;; (language javascript spec) --- GuileScript.

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

(define-module (language javascript spec)
  #:use-module (system base language)
  #:use-module (language javascript decompile-tree-il)
  #:export (javascript))

;;;
;;; Language definition
;;;

(define-language javascript
  #:title	"JavaScript"
  #:reader	(lambda (port env) (read port))
  #:decompilers `((tree-il . ,decompile-tree-il))
  #:printer	write)
