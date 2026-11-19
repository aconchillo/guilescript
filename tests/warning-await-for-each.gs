;; for-each doesn't wait for async functions, so it's a warning (the code
;; still compiles).
(define (save-all! items)
  (for-each (lambda (item) (js-await (.resolve Promise item))) items))
;; warning: warning-await-for-each.gs:4
;; warning: for-each doesn't wait for it
