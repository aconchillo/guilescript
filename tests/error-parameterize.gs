;; Errors in macro expansions point to the user's code.
(define p (js-obj))
(parameterize ((p 2)) (p))
;; error: error-parameterize.gs:3
;; error: is not supported by GuileScript
