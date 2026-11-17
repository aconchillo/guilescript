;; -*- mode: scheme; coding: utf-8; -*-

(define-module (main)
  #:use-module (counter))

(js-global document)

(.append-child (.-body document) (make-counter-button "Clicks"))
