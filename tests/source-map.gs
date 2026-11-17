;; -*- mode: scheme; coding: utf-8; -*-

;;
;; Source maps: stack traces point to the GuileScript code (tests are
;; compiled with --source-map and run with node --enable-source-maps).
;;

(define (location-in-stack e)
  (let ((m (.match (.-stack e) (js-new RegExp "source-map\\.gs:(\\d+)"))))
    (if m (js-ref m 1) "no location")))

(define (fail-here)
  (error "boom"))

(console-log (guard (e (#t (location-in-stack e)))
               (fail-here)))
;; => 13

;; Inside a loop.
(define (find-negative xs)
  (let loop ((xs xs))
    (cond
     ((null? xs) #f)
     ((negative? (car xs))
      (raise (js-new Error "negative")))
     (else (loop (cdr xs))))))

(console-log (guard (e (#t (location-in-stack e)))
               (find-negative '(1 2 -3))))
;; => 25
