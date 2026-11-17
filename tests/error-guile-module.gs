(use-modules (ice-9 string-fun))
(console-log (string-replace-substring "a-b" "-" "+"))
;; error: error-guile-module.gs:2
;; error: `string-replace-substring' from (ice-9 string-fun) is not supported by GuileScript
