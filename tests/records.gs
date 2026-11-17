;; -*- mode: scheme; coding: utf-8; -*-

;;
;; Records are JavaScript classes. Calls to record procedures are compiled
;; into JavaScript expressions.
;;

(define-record-type <point>
  (make-point x y)
  point?
  (x point-x set-point-x!)
  (y point-y))

(define p (make-point 1 2))
(console-log p (point-x p) (point-y p) (point? p) (point? 5))
;; => Point { x: 1, y: 2 } 1 2 true false

(set-point-x! p 10)
(console-log (point-x p))
;; => 10

;; Record procedures can be used as values.
(console-log (map point-y (list (make-point 1 2) (make-point 3 4))))
;; => [ 2, 4 ]

;; The record type is the JavaScript class.
(console-log (js-instanceof p <point>) (.-name <point>))
;; => true Point

;; Names with dashes, and fields not set by the constructor.
(define-record-type <http-request>
  (make-http-request method)
  http-request?
  (method http-request-method)
  (status-code http-request-status-code set-http-request-status-code!))

(define req (make-http-request "GET"))
(set-http-request-status-code! req 200)
(console-log req (http-request-status-code req))
;; => HttpRequest { method: 'GET', statusCode: 200 } 200

;; Records defined inside a function.
(define (distance a b)
  (define-record-type <vec>
    (make-vec dx dy)
    vec?
    (dx vec-dx)
    (dy vec-dy))
  (let ((v (make-vec (- (point-x b) (point-x a)) (- (point-y b) (point-y a)))))
    (sqrt (+ (* (vec-dx v) (vec-dx v)) (* (vec-dy v) (vec-dy v))))))

(console-log (distance (make-point 0 0) (make-point 3 4)))
;; => 5

;; Records can be exported.
(export <point> make-point point?)
