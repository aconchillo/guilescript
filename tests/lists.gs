;; -*- mode: scheme; coding: utf-8; -*-

;;
;; Lists and vectors are JavaScript arrays.
;;

(define xs '(1 2 3 4 5))

(console-log (car xs) (cdr xs) (cadr xs) (length xs) (list-ref xs 2))
;; => 1 [ 2, 3, 4, 5 ] 2 5 3

(console-log (cons 0 xs) (append '(a) '(1 2) '(z)) (reverse xs))
;; => [ 0, 1, 2, 3, 4, 5 ] [ 'a', 1, 2, 'z' ] [ 5, 4, 3, 2, 1 ]

(console-log (append) (append xs) (apply + xs) (sort '(b c a) string<?))
;; => [] [ 1, 2, 3, 4, 5 ] 15 [ 'a', 'b', 'c' ]

(console-log (null? '()) (null? xs) (pair? xs) (list? xs))
;; => true false true true

;; Arrays are never ===, so comparing with '() checks for an empty list.
(define none '())
(console-log (eq? none '()) (equal? '() xs) (eqv? none '()))
;; => true false true

(console-log (map (lambda (x) (* x x)) xs))
;; => [ 1, 4, 9, 16, 25 ]

;; map and for-each with several lists.
(console-log (map + '(1 2) '(10 20)) (map list '(1 2) '(a b) '(x y)))
;; => [ 11, 22 ] [ [ 1, 'a', 'x' ], [ 2, 'b', 'y' ] ]

(for-each (lambda (k v) (console-log k v)) '(a b) '(1 2))
;; => a 1
;; => b 2

(console-log (filter odd? xs) (remove odd? xs))
;; => [ 1, 3, 5 ] [ 2, 4 ]

(console-log (fold + 0 xs) (fold cons '() '(1 2 3)) (reduce max 0 xs))
;; => 15 [ 3, 2, 1 ] 5

(console-log (any negative? xs) (every positive? xs) (find even? xs) (count even? xs))
;; => false true 2 2

(console-log (iota 5) (iota 3 1) (list-tail xs 3) (delete 3 xs))
;; => [ 0, 1, 2, 3, 4 ] [ 1, 2, 3 ] [ 4, 5 ] [ 1, 2, 4, 5 ]

(console-log (sort '(3 1 2) <) (member 3 xs) (delete-duplicates '(1 1 2 1)))
;; => [ 1, 2, 3 ] true [ 1, 2 ]

(console-log (vector 1 2) (vector-length #(1 2 3)) (make-vector 3 0))
;; => [ 1, 2 ] 3 [ 0, 0, 0 ]

;; Vector literals are constants (like in Guile): names inside are symbols.
(define age 34)
(console-log #(1 2 age) `#(1 2 ,age) #(1 #(2 "x")))
;; => [ 1, 2, 'age' ] [ 1, 2, 34 ] [ 1, [ 2, 'x' ] ]

(define v (vector 1 2 3))
(vector-set! v 0 'x)
(console-log v (vector-ref v 0))
;; => [ 'x', 2, 3 ] x

(for-each (lambda (x) (console-log "item" x)) '(a b))
;; => item a
;; => item b

;;
;; Quasiquote
;;

(define n 2)
(define more '(3 4))
(console-log `(1 ,n ,@more 5) `(a (b ,n)))
;; => [ 1, 2, 3, 4, 5 ] [ 'a', [ 'b', 2 ] ]

;;
;; Objects are created with js-obj (quoted data is always arrays: JavaScript
;; has no pairs). assoc-ref reads their properties.
;;

(define person (js-obj 'name "Ann" #:age 30))
(console-log person (assoc-ref person 'name))
;; => { name: 'Ann', age: 30 } Ann

;; A list of lists can be iterated.
(define filters '(("all" "All") ("done" "Done")))
(console-log (map car filters) (map cadr filters))
;; => [ 'all', 'done' ] [ 'All', 'Done' ]

;;
;; Hash tables are Maps.
;;

(define h (make-hash-table))
(hash-set! h 'a 1)
(hash-set! h 'b 2)
(console-log (hash-ref h 'a) (hash-ref h 'z 'none) (hash-ref h 'z))
;; => 1 none undefined

(hash-remove! h 'a)
(console-log (hash-map->list (lambda (k v) (list k v)) h))
;; => [ [ 'b', 2 ] ]
