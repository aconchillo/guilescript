;; -*- mode: scheme; coding: utf-8; -*-

(define-module (counter)
  #:export (make-counter-button))

(js-global document)

;; A button that counts how many times it's been clicked.
(define (make-counter-button label)
  (let ((button (.create-element document "button"))
        (count 0))
    (define (render!)
      (set! (.-text-content button)
            (string-append label ": " (number->string count))))
    (.add-event-listener button "click"
                         (lambda (event)
                           (set! count (+ count 1))
                           (render!)))
    (render!)
    button))
