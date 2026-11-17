;; -*- mode: scheme; coding: utf-8; -*-

;;
;; Binary search using vectors.
;;
(define (binary-search v x)
  (let loop ((l 0)
             (r (- (vector-length v) 1)))
    (cond
     ((<= l r)
      (let ((m (floor (/ (+ l r) 2))))
        (cond
         ((= (vector-ref v m) x) m)
         ((< (vector-ref v m) x) (loop (+ m 1) r))
         (else (loop l (- m 1))))))
     (else -1))))

(console-log (binary-search #(1 3 5 7 9 11) 7))
(console-log (binary-search #(1 3 5 7 9 11) 4))
;; => 3
;; => -1
