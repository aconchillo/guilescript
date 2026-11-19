;; -*- mode: scheme; coding: utf-8; -*-

;;
;; Reverse a vector in place.
;;
(define (reverse-vector! v)
  (let loop ((l 0)
             (r (- (vector-length v) 1)))
    (cond
     ((< l r)
      (let ((c (vector-ref v l)))
        (vector-set! v l (vector-ref v r))
        (vector-set! v r c)
        (loop (+ l 1) (- r 1))))
     (else v))))

(console-log (.stringify JSON (reverse-vector! (vector 1 2 3 4 5))))
;; => [5,4,3,2,1]
