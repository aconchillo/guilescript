;; -*- mode: scheme; coding: utf-8; -*-

;;
;; Create a gs-when macro, same as when, to simply show that macros work.
;;
(define-syntax gs-when
  (syntax-rules ()
    ((gs-when condition exp ...)
     (if condition
         (begin exp ...)))))

(gs-when #t
  (console-log "hey ho")
  (console-log "let's go"))
