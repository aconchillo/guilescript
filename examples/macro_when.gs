;; -*- mode: scheme; coding: utf-8; -*-

;;
;; Create a my-when macro, same as when, to simply show that macros work.
;;
(define-syntax my-when
  (syntax-rules ()
    ((my-when condition exp ...)
     (if condition
         (begin exp ...)))))

(my-when #t
  (log:info "hey ho")
  (log:info "let's go"))
