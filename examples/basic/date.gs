;; -*- mode: scheme; coding: utf-8; -*-

;;
;; JavaScript classes and methods: the current year.
;;
(define today (js-new Date))

(console-log (> (.get-full-year today) 2000))
;; => true
