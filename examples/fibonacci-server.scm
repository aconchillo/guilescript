;; -*- mode: scheme; coding: utf-8; -*-

;;
;; This is a web server implemented in Guile. The web server uses GuileScript to
;; generate JavaScript code that will be used in the returned page.
;;
;; Run with: ./env guile examples/fibonacci-server.scm
;;


(use-modules ((language guilescript compile) #:prefix gs:)
             (ice-9 match)
             (ice-9 receive)
             (sxml simple)
             (web request)
             (web response)
             (web server)
             (web uri))

;;
;; GuileScript handler.
;;
(define js-fibonacci
  '(begin
     (define (fib n)
       (if (<= n 1)
           1
           (+ (fib (- n 2)) (fib (- n 1)))))

     (define (computeFibonacci)
       (let ((elem (js-invoke document 'getElementById "number"))
             (n (js-ref elem 'value)))
         (alert (+ "Fibonacci of " n " is " (fib n)))))))

(define js-script
  (receive (js _)
      (gs:compile js-fibonacci)
    js))

;;
;; This is the web page.
;;
(define (main-form request body)
  `(html
    (@ (xmlns "http://www.w3.org/1999/xhtml"))
    (head (title "Fibonacci GuileScript Server"))
    (script ,js-script)
    (body
     (input (@ (id "number") (type "text") (size "50") (value "")))
     (button (@ (type "button") (onclick "computeFibonacci()")) "Compute Fibonacci"))))

(define (main-form-handler request body)
  (values (build-response
           #:headers '((content-type . (application/xhtml+xml))))
          (lambda (port)
            (sxml->xml (main-form request body) port))))

;;
;; 404
;;
(define (not-found)
  (values (build-response #:code 404)
          "Resource not found"))

;;
;; Web server
;;
(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define (main-handler request body)
  (match (cons (request-method request)
               (request-path-components request))
    (('GET) (main-form-handler request body))
    (_ (not-found))))

(display "\nNow go to http://127.0.0.1:8080\n")

(run-server main-handler)
