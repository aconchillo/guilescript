;; -*- mode: scheme; coding: utf-8; -*-

;;
;; JavaScript interoperability.
;;

;; Keywords are camelCased, symbols and strings are used as they are.
(define obj (js-obj #:first-name "Ann" 'last-name "Lee" "age" 30))
(console-log obj)
;; => { firstName: 'Ann', 'last-name': 'Lee', age: 30 }

(console-log (js-ref obj 'firstName) (js-ref obj "last-name"))
;; => Ann Lee

(js-set! obj 'age 31)
(console-log (js-ref obj 'age))
;; => 31

(define nested (js-obj 'a (js-obj 'b (js-obj 'c 42))))
(console-log (js-ref nested 'a 'b 'c))
;; => 42

(console-log (js-invoke "a,b" 'split ",") (js-invoke (js-invoke "  x " 'trim) 'toUpperCase))
;; => [ 'a', 'b' ] X

(define d (js-new Date 0))
(console-log (js-invoke d 'getTime))
;; => 0

(console-log (js-typeof 1) (js-typeof "s") (js-instanceof d Date))
;; => number string true

;; Trailing keyword arguments become an options object.
(define (describe options)
  (string-append (js-ref options 'name) "/" (number->string (js-ref options 'size))))

(console-log (describe #:name "box" #:size 3))
;; => box/3

(console-log (js-invoke JSON 'stringify (js-obj 'xs (list 1 2))))
;; => {"xs":[1,2]}

(console-log (js-ref Math 'PI))
;; => 3.141592653589793

(js-delete! obj 'age)
(console-log (js-invoke Object 'keys obj))
;; => [ 'firstName', 'last-name' ]

;; Unknown names are JavaScript globals.
(console-log (parseFloat "1.5") (js-typeof setTimeout))
;; => 1.5 function

