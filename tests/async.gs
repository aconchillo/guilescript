;; -*- mode: scheme; coding: utf-8; -*-

;;
;; js-await: functions using it become async functions. At the top level of
;; an ES module it is a top-level await.
;;

(define (later ms value)
  (js-new Promise (lambda (resolve)
                    (set-timeout (lambda () (resolve value)) ms))))

(define (add-later a b)
  (+ (js-await (later 5 a)) (js-await (later 5 b))))

(console-log "add-later:" (js-await (add-later 1 2)))
;; => add-later: 3

;; Awaiting inside a loop.
(define (sum-later xs)
  (let loop ((xs xs) (total 0))
    (if (null? xs)
        total
        (loop (cdr xs) (+ total (js-await (later 1 (car xs))))))))

(console-log "sum-later:" (js-await (sum-later '(1 2 3))))
;; => sum-later: 6

;; Async lambdas, e.g. with Promise.all.
(console-log (js-await (.all Promise (map (lambda (x) (* 10 (js-await (later 1 x))))
                                          '(1 2 3)))))
;; => [ 10, 20, 30 ]

;; Async functions return promises.
(.then (add-later 3 4) (lambda (v) (console-log "then:" v)))
;; => then: 7
