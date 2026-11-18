;;; (language javascript errors) --- GuileScript.

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

;; Compilation errors. They are thrown with the 'guilescript-error key and
;; two arguments: the error message and the source location (an alist with
;; filename, line and column, or #f).

;;; Code:

(define-module (language javascript errors)
  #:use-module (ice-9 format)
  #:export (compile-error
            compile-error-at
            compile-warning
            call-with-source-file
            user-location?
            note-location!
            reset-location!
            current-location
            format-location))

(define %location (make-thread-local-fluid #f))
(define %file (make-thread-local-fluid #f))

;; Compile THUNK knowing that the code comes from FILE. Locations of code
;; coming from other files (i.e. macro definitions) are then ignored.
(define (call-with-source-file file thunk)
  (with-fluids ((%file file))
    (thunk)))

(define (user-location? src)
  (and src
       (let ((file (fluid-ref %file)))
         (or (not file) (equal? (assq-ref src 'filename) file)))))

(define (reset-location!)
  (fluid-set! %location #f))

(define (current-location)
  (fluid-ref %location))

;; Remember SRC (a Tree-IL source location) as the most recent location seen
;; by the compiler.
(define (note-location! src)
  (when (user-location? src)
    (fluid-set! %location src)))

;; A compilation error at the source location SRC.
(define (compile-error-at src fmt . args)
  (throw 'guilescript-error (apply format #f fmt args) src))

;; A compilation error at the most recent location seen by the compiler.
(define (compile-error fmt . args)
  (apply compile-error-at (current-location) fmt args))

(define (compile-warning loc fmt . args)
  (format (current-warning-port) "~a: warning: ~a~%"
          (or (format-location (if (user-location? loc) loc (current-location)))
              "guilescript")
          (apply format #f fmt args)))

;; "file.gs:3:5" (lines are 1-based, columns 0-based like Guile).
(define (format-location loc)
  (and loc
       (let ((file (assq-ref loc 'filename))
             (line (assq-ref loc 'line))
             (column (assq-ref loc 'column)))
         (format #f "~a:~a:~a"
                 (or file "<unknown>")
                 (if line (+ line 1) "?")
                 (or column "?")))))
