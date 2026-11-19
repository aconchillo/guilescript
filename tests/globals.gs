;; -*- mode: scheme; coding: utf-8; -*-

;;
;; Standard JavaScript globals can be used directly. Other globals (from the
;; browser or Node.js) have to be declared with js-global.
;;

(js-global process)

(console-log (js-typeof (.-argv process)) (parse-int "42") (.floor Math 2.5))
;; => object 42 2

;; Declared names follow the usual naming rules: set-immediate => setImmediate
(js-global set-immediate)

(set-immediate (lambda () (console-log "later")))
(console-log "now")
;; => now
;; => later
