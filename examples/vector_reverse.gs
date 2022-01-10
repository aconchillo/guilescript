;; -*- mode: scheme; coding: utf-8; -*-

;;
;; Vector reverse.
;;
(define (vector_reverse v)
  (let loop ((l 0)
             (r (- (vector-length v) 1)))
    (cond
     ((< l r)
      (let ((c (vector-ref v l)))
        (vector-set! v l (vector-ref v r))
        (vector-set! v r c)
        (loop (+ l 1) (- r 1))))
     (else v))))
