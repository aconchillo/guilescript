;; -*- mode: scheme; coding: utf-8; -*-

(define s "Hello")

(console-log (string-length s) (string-ref s 1) (substring s 1 3))
;; => 5 e el

(console-log (string-append s ", " "world" "!"))
;; => Hello, world!

(console-log (string-upcase s) (string-downcase s))
;; => HELLO hello

(console-log (string-prefix? "He" s) (string-suffix? "lo" s) (string-null? ""))
;; => true true true

(console-log (string-join (list "a" "b" "c") "-") (string-join (string-split "x,y" #\,)))
;; => a-b-c x y

(console-log (list->string (reverse (string->list "abc"))))
;; => cba

(console-log (string=? "a" "a") (string<? "a" "b") (char->integer #\A) (integer->char 97))
;; => true true 65 a

(console-log (string-trim-both "  x  ") (symbol->string 'abc) (string->symbol "abc"))
;; => x abc abc

;; Control characters are escaped.
(console-log (string-length "a\x01b") (char->integer (string-ref "a\x01b" 1)))
;; => 3 1
