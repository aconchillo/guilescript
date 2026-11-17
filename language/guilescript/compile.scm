;;; (language guilescript compile) --- GuileScript.

;; Copyright (C) 2022-2026 Aleix Conchillo Flaque <aconchillo@gmail.com>
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

;; GuileScript compiler

;;; Code:

(define-module (language guilescript compile)
  #:use-module ((system base compile) #:prefix system:)
  #:use-module ((system base language) #:select (default-environment))
  #:use-module (language javascript errors)
  #:use-module (language guilescript modules)
  #:export (compile compile-file compile-module-file))

;; The code is expanded in a fresh module (ENV), which is also given to the
;; JavaScript compiler so it knows where imported names come from.

(define (compile exp)
  (let ((env (default-environment 'guilescript)))
    (system:decompile
     (system:compile exp #:from 'guilescript #:to 'tree-il #:env env)
     #:from 'tree-il #:to 'javascript #:env env)))

;; Compile FILENAME into JavaScript. Returns the JavaScript code, the path of
;; the compiled module (without extension, e.g. "app/util" for the module
;; (app util)) and the GuileScript modules it imports: ((NAME . FILE) ...).
;; GuileScript modules are also searched in the LOAD-PATH directories.
(define* (compile-module-file filename #:key (load-path '()))
  (let ((ctx (make-module-context filename load-path)))
    (call-with-source-file filename
      (lambda ()
        (parameterize ((current-module-context ctx))
          (call-with-input-file filename
            (lambda (p)
              (let* ((env (default-environment 'guilescript))
                     (tree-il (system:read-and-compile p #:from 'guilescript #:to 'tree-il
                                                       #:env env))
                     (js (call-with-values
                             (lambda ()
                               (system:decompile tree-il #:from 'tree-il #:to 'javascript
                                                 #:env env))
                           (lambda (js . _) js))))
                (values js
                        (module-context-path ctx)
                        (module-context-dependencies ctx))))))))))

(define* (compile-file filename #:key (load-path '()))
  (call-with-values (lambda () (compile-module-file filename #:load-path load-path))
    (lambda (js path dependencies) js)))
