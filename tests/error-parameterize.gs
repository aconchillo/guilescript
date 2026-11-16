(define p (make-parameter 1))
(parameterize ((p 2)) (p))
;; error: error-parameterize.gs:2
;; error: is not supported by GuileScript
