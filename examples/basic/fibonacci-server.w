;; -*- mode: wisp; coding: utf-8; -*-

;;
;; This is the wisp version of the web server and a web application
;; written in Guile in fibonacci-server.scm.
;; The web application computes a fibonacci number. The web server
;; uses GuileScript to generate the JavaScript code (that computes a
;; fibonacci number) that will embedded inline in a <script> tag.
;;
;; If you are building from source, compile with make, then run with:
;;
;;    ./env wisp examples/basic/fibonacci-server.w
;;
;; If GuileScript is already installed, compile with make, then run with:
;;
;;    wisp examples/basic/fibonacci-server.w
;;
;; Guile 3.0.10 or later can also run it without the wisp command:
;;
;;    guile --language=wisp -s examples/basic/fibonacci-server.w
;;

use-modules : (language guilescript compile) #:prefix gs:
              ice-9 match
              ice-9 receive
              sxml simple
              web request
              web response
              web server
              web uri

;;
;; GuileScript handler.
;;
define js-fibonacci
  ' begin
      js-global document alert

      define (fib n)
        if (<= n 1)
           . 1
           + (fib (- n 2)) (fib (- n 1))

      ;; computeFibonacci() in JavaScript.
      define (compute-fibonacci)
        let ((n (string->number (.-value (.get-element-by-id document "number")))))
          alert (string-append "Fibonacci of " (number->string n) " is " (number->string (fib n)))


define js-script
  receive (js _)
    gs:compile js-fibonacci
    . js

;;
;; This is the web page.
;;
define (main-form request body)
  `
   html
    @ : xmlns "http://www.w3.org/1999/xhtml"
    head : title "Fibonacci GuileScript Server"
    script ,js-script
    body
      input : @ (id "number") (type "text") (size "50") (value "")
      button : @ (type "button") (onclick "computeFibonacci()")
        . "Compute Fibonacci"

define (main-form-handler request body)
  values
      build-response
          . #:headers '((content-type . (application/xhtml+xml)))
      lambda (port)
          sxml->xml (main-form request body) port

;;
;; 404
;;
define (not-found)
   values (build-response #:code 404)
          . "Resource not found"

;;
;; Web server
;;
define (request-path-components request)
  split-and-decode-uri-path (uri-path (request-uri request))

define (main-handler request body)
  match : cons (request-method request) (request-path-components request)
    ('GET) (main-form-handler request body)
    _ (not-found)

display "\nNow go to http://127.0.0.1:8080\n"

run-server main-handler
