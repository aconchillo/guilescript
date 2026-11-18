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
  #:use-module (ice-9 textual-ports)
  #:use-module (language javascript errors)
  #:use-module (language javascript source-map)
  #:use-module (language guilescript modules)
  #:export (compile compile-file compile-module-file compile-modules))

;; The code is expanded in a fresh module (ENV), which is also given to the
;; JavaScript compiler so it knows where imported names come from.
;;
;; Guile's warnings while expanding are about Guile's semantics (e.g. case
;; with strings, which works in GuileScript), so they are not shown.
;; GuileScript reports its own problems.

(define (without-guile-warnings thunk)
  (parameterize ((current-warning-port (%make-void-port "w")))
    (thunk)))

(define (compile exp)
  (let ((env (default-environment 'guilescript)))
    (system:decompile
     (without-guile-warnings
      (lambda () (system:compile exp #:from 'guilescript #:to 'tree-il #:env env)))
     #:from 'tree-il #:to 'javascript #:env env)))

;; Compile FILENAME into JavaScript. Returns the JavaScript code, the path of
;; the compiled module (without extension, e.g. "app/util" for the module
;; (app util)) and the GuileScript modules it imports: ((NAME . FILE) ...).
;; GuileScript modules are also searched in the LOAD-PATH directories. With
;; SOURCE-IMPORTS?, GuileScript modules are imported by the path of their .gs
;; file (for bundler plugins) instead of the path of the compiled module.
;; The LOAD-PATH directories are also added to Guile's load path while
;; compiling, so Guile modules (e.g. with macros) are also found there.
(define* (compile-module-file filename #:key (load-path '()) (source-imports? #f))
  (let ((ctx (make-module-context filename load-path #:source-imports? source-imports?))
        (saved-load-path %load-path))
    (dynamic-wind
      (lambda () (set! %load-path (append load-path %load-path)))
      (lambda () (compile-module-file* ctx filename))
      (lambda () (set! %load-path saved-load-path)))))

(define (compile-module-file* ctx filename)
    (call-with-source-file filename
      (lambda ()
        (parameterize ((current-module-context ctx))
          (call-with-input-file filename
            (lambda (p)
              (let* ((env (default-environment 'guilescript))
                     (tree-il (without-guile-warnings
                               (lambda ()
                                 (system:read-and-compile p #:from 'guilescript #:to 'tree-il
                                                          #:env env))))
                     (js (call-with-values
                             (lambda ()
                               (system:decompile tree-il #:from 'tree-il #:to 'javascript
                                                 #:env env))
                           (lambda (js . _) js))))
                (values js
                        (module-context-path ctx)
                        (module-context-dependencies ctx)))))))))

(define* (compile-file filename #:key (load-path '()) (source-imports? #f))
  (call-with-values (lambda () (compile-module-file filename #:load-path load-path
                                                    #:source-imports? source-imports?))
    (lambda (js path dependencies) js)))

;; Compile FILES and all the GuileScript modules they import (each one only
;; once), e.g. to serve them from a Guile web server. Returns a list of
;; (PATH JS SOURCE-MAP), where PATH is the path of the compiled module
;; without extension (e.g. "tasks/api") and SOURCE-MAP is the source map (as
;; JSON) if SOURCE-MAP? is true, or #f. SOURCE-MAP-SOURCE is called with the
;; compiled file and the module path, and returns the source of the source
;; map (by default the name of the .gs file, i.e. next to the compiled
;; module). Source maps include the source code.
(define* (compile-modules files #:key (load-path '()) (source-map? #f)
                          (source-map-source (lambda (file path) (basename file))))
  (let lp ((pending files) (done '()) (results '()))
    (if (null? pending)
        (reverse results)
        (let* ((file (car pending))
               (key (canonicalize-path file)))
          (if (member key done)
              (lp (cdr pending) done results)
              (call-with-values
                  (lambda ()
                    (call-with-mappings source-map?
                      (lambda () (compile-module-file file #:load-path load-path))))
                (lambda (js path dependencies mappings)
                  (lp (append (cdr pending) (map cdr dependencies))
                      (cons key done)
                      (cons (list path
                                  js
                                  (and mappings
                                       (source-map-json
                                        mappings
                                        #:file (string-append (basename path) ".js")
                                        #:source (source-map-source file path)
                                        #:content (call-with-input-file file get-string-all))))
                            results)))))))))
