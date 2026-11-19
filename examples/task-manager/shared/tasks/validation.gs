;; -*- mode: scheme; coding: utf-8; -*-

;;
;; Task validation, shared by the server (which loads this file as a Guile
;; module) and the clients (which compile it into JavaScript). So it only uses
;; what works the same way in Guile and GuileScript.
;;

(define-module (tasks validation)
  #:export (max-title-length
            task-title-errors))

(define max-title-length 100)

;; The list of problems with TITLE (empty if it's a valid title).
(define (task-title-errors title)
  (cond
   ((not (string? title))
    (list "The title must be a string."))
   ((string-null? (string-trim-both title))
    (list "The title can't be empty."))
   ((> (string-length title) max-title-length)
    (list (string-append "The title can't be longer than "
                         (number->string max-title-length)
                         " characters.")))
   (else '())))
