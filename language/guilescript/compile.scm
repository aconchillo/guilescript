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
  #:use-module ((system base compile) #:prefix system:)
  #:export (compile compile-file))

(define (compile exp)
  (system:decompile
   (system:compile exp #:from 'guilescript #:to 'tree-il)
   #:from 'tree-il #:to 'javascript))

(define (compile-file filename)
  (call-with-input-file
      filename
    (lambda (p)
      (let ((tree-il (system:read-and-compile p #:from 'guilescript #:to 'tree-il)))
        (system:decompile tree-il #:from 'tree-il #:to 'javascript)))))
