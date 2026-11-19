;; -*- mode: scheme; coding: utf-8; -*-

;;
;; ES modules: js-import, define-public and export.
;;

(js-import "node:path" join basename)
(js-import "node:path" #:default path)
(js-import "node:os" #:as os)

(console-log (join "a" "b") (basename "/x/y.txt") (js-invoke path 'extname "f.js"))
;; => a/b y.txt .js

(console-log (js-typeof (js-ref os 'platform)))
;; => function

(define-public (square x)
  (* x x))

(define (cube x)
  (* x x x))

(export cube)

(console-log (square 3) (cube 2))
;; => 9 8

;; Imported names can be renamed.
(js-import "node:path" #:rename '((join . path-join) (sep . path-separator)))

(console-log (path-join "x" "y") path-separator)
;; => x/y /
