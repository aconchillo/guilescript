(define-record-type <point> (make-point x y) point? (x point-x))
(make-point 1)
;; error: error-record-arity.gs:2
;; error: wrong number of arguments to a record procedure
