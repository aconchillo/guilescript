;; (ice-9 format) replaces format from Guile core.
(use-modules (ice-9 format))
(console-log (format #f "~a" 1))
;; error: `format' from (ice-9 format) is not supported by GuileScript
