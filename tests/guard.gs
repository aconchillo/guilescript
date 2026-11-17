;; -*- mode: scheme; coding: utf-8; -*-

;;
;; guard (R7RS) is compiled into try/catch.
;;

(define (safe-div a b)
  (guard (e ((string? e) (string-append "error: " e))
            ((error-object? e) (string-append "Error: " (error-object-message e))))
    (when (= b 0)
      (raise "division by zero"))
    (/ a b)))

(console-log (safe-div 10 2) (safe-div 1 0))
;; => 5 error: division by zero

(define (parse s)
  (guard (e (#t #f))
    (.parse JSON s)))

(console-log (parse "[1]") (parse "{bad"))
;; => [ 1 ] false

(console-log (guard (e ((error-object? e) (error-object-message e)))
               (error "bad thing:" 42)))
;; => bad thing: 42

;; If no clause matches, the exception is raised again.
(console-log (guard (outer (#t (string-append "outer: " outer)))
               (guard (inner ((number? inner) "number"))
                 (raise "not a number"))))
;; => outer: not a number

;; Clauses with => and else.
(define (code-of thunk)
  (guard (e ((assoc-ref e 'code) => (lambda (code) (* code 10)))
            (else "no code"))
    (thunk)))

(console-log (code-of (lambda () (raise (js-obj 'code 4))))
             (code-of (lambda () (raise (js-obj 'x 1)))))
;; => 40 no code

;; guard as a value in a loop.
(define (first-parsable strings)
  (let loop ((strings strings))
    (if (null? strings)
        'none
        (let ((value (guard (e (#t #f)) (.parse JSON (car strings)))))
          (if value value (loop (cdr strings)))))))

(console-log (first-parsable '("{" "[2]" "3")) (first-parsable '("{")))
;; => [ 2 ] none

;; guard with js-await.
(define (fetch-number promise)
  (guard (e (#t (string-append "failed: " (error-object-message e))))
    (* 2 (js-await promise))))

(console-log (js-await (fetch-number (.resolve Promise 21)))
             (js-await (fetch-number (.reject Promise (js-new Error "nope")))))
;; => 42 failed: nope
