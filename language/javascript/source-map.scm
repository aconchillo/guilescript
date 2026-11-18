;;; (language javascript source-map) --- GuileScript.

;; Copyright (C) 2026 Aleix Conchillo Flaque <aconchillo@gmail.com>
;;
;; This file is part of GuileScript.
;;
;; GuileScript is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; GuileScript is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GuileScript. If not, see https://www.gnu.org/licenses/.

;;; Commentary:

;; Source maps (version 3, https://tc39.es/ecma426/).
;;
;; A mapping is a list (GENERATED-LINE GENERATED-COLUMN SOURCE-LINE
;; SOURCE-COLUMN), all of them 0-based.

;;; Code:

(define-module (language javascript source-map)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (language javascript print)
  #:export (current-mappings-receiver
            call-with-mappings
            source-map-json
            base64-encode))

;; A procedure that receives the mappings of the compiled code, or #f if no
;; source map is needed.
(define current-mappings-receiver (make-parameter #f))

;; Call THUNK, which compiles code, and return its values followed by the
;; source map mappings of the compiled code (or #f if SOURCE-MAP? is false).
(define (call-with-mappings source-map? thunk)
  (let ((mappings #f))
    (call-with-values
        (lambda ()
          (parameterize ((current-mappings-receiver
                          (and source-map? (lambda (m) (set! mappings m)))))
            (thunk)))
      (lambda results
        (apply values (append results (list mappings)))))))

(define base64-digits
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

;; Base64 VLQ encoding of the integer N.
(define (vlq n)
  (let lp ((v (if (negative? n) (+ 1 (* 2 (- n))) (* 2 n)))
           (digits '()))
    (let ((digit (logand v 31))
          (rest (ash v -5)))
      (if (zero? rest)
          (list->string (reverse (cons (string-ref base64-digits digit) digits)))
          (lp rest (cons (string-ref base64-digits (logior digit 32)) digits))))))

;; The "mappings" field: one group of segments per generated line. Each
;; segment has the generated column, the source index (always 0, there's a
;; single source), the source line and the source column, all of them
;; relative to the previous segment.
(define (encode-mappings mappings)
  (if (null? mappings)
      ""
      (let* ((lines (+ 1 (apply max (map first mappings))))
             (by-line (make-vector lines '())))
        (for-each (lambda (m)
                    (vector-set! by-line (first m) (cons m (vector-ref by-line (first m)))))
                  mappings)
        (let lp ((line 0) (source-line 0) (source-column 0) (groups '()))
          (if (= line lines)
              (string-join (reverse groups) ";")
              (let segments ((ms (sort (vector-ref by-line line)
                                       (lambda (a b) (< (second a) (second b)))))
                             (column 0)
                             (source-line source-line)
                             (source-column source-column)
                             (encoded '()))
                (match ms
                  (()
                   (lp (+ line 1) source-line source-column
                       (cons (string-join (reverse encoded) ",") groups)))
                  (((_ c sl sc) . rest)
                   (segments rest c sl sc
                             (cons (string-append (vlq (- c column))
                                                  (vlq 0)
                                                  (vlq (- sl source-line))
                                                  (vlq (- sc source-column)))
                                   encoded))))))))))

;; The source map, as JSON, of the code generated from SOURCE (a path or URL,
;; relative to the source map). FILE is the generated file and CONTENT the
;; content of SOURCE.
(define* (source-map-json mappings #:key file source content)
  (string-append
   "{\"version\":3"
   (if file (string-append ",\"file\":" (js-string-literal file)) "")
   ",\"sources\":[" (js-string-literal source) "]"
   (if content (string-append ",\"sourcesContent\":[" (js-string-literal content) "]") "")
   ",\"names\":[],\"mappings\":" (js-string-literal (encode-mappings mappings))
   "}"))

(define (base64-encode s)
  (let* ((bv (string->utf8 s))
         (n (bytevector-length bv))
         (byte (lambda (i) (if (< i n) (bytevector-u8-ref bv i) 0))))
    (call-with-output-string
      (lambda (port)
        (let lp ((i 0))
          (when (< i n)
            (let ((triple (logior (ash (byte i) 16) (ash (byte (+ i 1)) 8) (byte (+ i 2)))))
              (write-char (string-ref base64-digits (logand (ash triple -18) 63)) port)
              (write-char (string-ref base64-digits (logand (ash triple -12) 63)) port)
              (write-char (if (< (+ i 1) n)
                              (string-ref base64-digits (logand (ash triple -6) 63))
                              #\=)
                          port)
              (write-char (if (< (+ i 2) n)
                              (string-ref base64-digits (logand triple 63))
                              #\=)
                          port)
              (lp (+ i 3)))))))))
