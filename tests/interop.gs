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

;; Method calls: (.method obj arg ...) => obj.method(arg, ...)
(console-log (.toUpperCase "abc") (.slice '(1 2 3 4) 1 3) (.to-fixed 3.14159 2))
;; => ABC [ 2, 3 ] 3.14

(console-log (.join (.map '(1 2) (lambda (x) (* x 10))) "-"))
;; => 10-20

;; Reserved words are fine as method names.
(define m (make-hash-table))
(hash-set! m 'k 1)
(.delete m 'k)
(console-log (js-ref m 'size))
;; => 0

;; Keyword arguments work like in any other call.
(console-log (.assign Object (js-obj 'a 1) #:b 2))
;; => { a: 1, b: 2 }

;; Methods can be used as values.
(define upcase .toUpperCase)
(console-log (map .toUpperCase '("a" "b")) (upcase "c"))
;; => [ 'A', 'B' ] C

;; Property access: (.-prop obj) => obj.prop
(define point (js-obj 'x 1 #:max-value 10))
(console-log (.-x point) (.-max-value point) (.-length '(1 2 3)) (map .-length '("a" "bb")))
;; => 1 10 3 [ 1, 2 ]

;; Properties can be set with set!, and so can js-ref paths.
(set! (.-x point) 5)
(set! (.-pos point) (js-obj))
(set! (js-ref point 'pos 'y) 6)
(console-log point)
;; => { x: 5, maxValue: 10, pos: { y: 6 } }
