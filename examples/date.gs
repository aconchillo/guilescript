;; -*- mode: scheme; coding: utf-8; -*-

;;
;; Prints current date and year.
;;
(define today (js-new Date))

(console-log today)
(console-log (js-invoke today getFullYear))
