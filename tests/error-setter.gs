(define l (list 1 2))
(set! (car l) 3)
;; error: set! is only supported on variables, (.-prop obj) and (js-ref obj ...)
