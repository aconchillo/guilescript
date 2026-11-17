;; -*- mode: scheme; coding: utf-8; -*-

(define-module (main)
  #:use-module (guilescript html)
  #:use-module (app))

(js-import "react" #:rename '((createElement . h)))
(js-import "react-dom/client" create-root)
(js-import "../../style.css")
(js-global document)

(.render (create-root (.get-element-by-id document "root"))
         (html (<app>)))
