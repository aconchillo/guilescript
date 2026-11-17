;;; (language guilescript modules) --- GuileScript.

;; Copyright (C) 2026 Aleix Conchillo Flaque <aconchillo@gmail.com>
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

;; GuileScript modules.
;;
;; A GuileScript module is a .gs file with a define-module form, e.g. the
;; module (app util) is the file app/util.gs. Each GuileScript module is
;; compiled into an ES module (app/util.js).
;;
;; define-module and use-modules forms are rewritten before Guile sees them
;; (Guile would load the imported modules, i.e. run them):
;;
;; - GuileScript modules are imported with js-import, so the names they
;;   export are imported from the JavaScript module. The import path is the
;;   path of the compiled module (e.g. ./util.js) or, with source imports
;;   (for bundler plugins), the path of the .gs file (e.g. ./util.gs).
;;
;; - Other modules are Guile modules and they are imported with use-modules
;;   (so their macros can be used).
;;
;; - #:export becomes export.

;;; Code:

(define-module (language guilescript modules)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:export (make-module-context
            module-context-path
            module-context-dependencies
            current-module-context
            transform-module-form))

(define-record-type <module-context>
  (%make-module-context file load-path source-imports? name root dependencies)
  module-context?
  (file module-context-file)
  (load-path module-context-load-path)
  (source-imports? module-context-source-imports?)
  (name module-context-name set-module-context-name!)
  ;; The directory where module paths start.
  (root module-context-root set-module-context-root!)
  ;; GuileScript modules imported: ((NAME . FILE) ...)
  (dependencies %module-context-dependencies set-module-context-dependencies!))

;; FILE is the file being compiled and LOAD-PATH a list of directories where
;; GuileScript modules are also searched.
(define* (make-module-context file load-path #:key (source-imports? #f))
  (%make-module-context file load-path source-imports? #f (dirname file) '()))

(define current-module-context (make-parameter #f))

(define (module-context-dependencies ctx)
  (reverse (%module-context-dependencies ctx)))

(define (module-error src fmt . args)
  (throw 'guilescript-error (apply format #f fmt args) src))

(define (module-name? x)
  (and (pair? x) (every symbol? x)))

(define (module-path name)
  (string-join (map symbol->string name) "/"))

;; The path of the compiled module, without extension (e.g. "app/util"). A
;; file without define-module is named after the file.
(define (module-context-path ctx)
  (match (module-context-name ctx)
    (#f (basename (module-context-file ctx) ".gs"))
    (name (module-path name))))

;; The module (a b c) must be in the file <root>/a/b/c.gs.
(define (set-module-name! ctx name src)
  (let* ((file (module-context-file ctx))
         (dir (dirname file))
         (parts (map symbol->string name))
         (dirs (drop-right parts 1)))
    (unless (string=? (basename file ".gs") (last parts))
      (module-error src "the module ~s should be in a file named ~a.gs" name (last parts)))
    (set-module-context-name! ctx name)
    (set-module-context-root!
     ctx
     (let lp ((dir dir) (dirs (reverse dirs)))
       (match dirs
         (() dir)
         ((d . rest)
          (unless (string=? (basename dir) d)
            (module-error src "the module ~s should be in the directory ~a"
                          name (string-join (map symbol->string (drop-right name 1)) "/")))
          (lp (dirname dir) rest)))))))

;; The .gs file of the module NAME, or #f if it's not a GuileScript module.
(define (module-file ctx name)
  (let ((relative (string-append (module-path name) ".gs")))
    (any (lambda (dir)
           (let ((file (string-append dir "/" relative)))
             (and (file-exists? file) file)))
         (cons (module-context-root ctx) (module-context-load-path ctx)))))

;; The names exported by the GuileScript module in FILE. The file is only
;; read (not run).
(define (module-exports file)
  (define (option-exports options)
    (match options
      (() '())
      (((or #:export #:re-export #:replace) (names ...) . rest)
       (append names (option-exports rest)))
      ((_ . rest) (option-exports rest))))
  (define (form-exports form)
    (match form
      (('define-module _ . options) (option-exports options))
      (('export names ...) names)
      (('define-public ((? symbol? name) . _) . _) (list name))
      (('define-public (? symbol? name) . _) (list name))
      (('begin forms ...) (append-map form-exports forms))
      (_ '())))
  (call-with-input-file file
    (lambda (port)
      (let lp ((exports '()))
        (let ((form (read port)))
          (if (eof-object? form)
              (delete-duplicates (filter symbol? exports))
              (lp (append exports (form-exports form)))))))))

;; The relative path from the directory FROM to TO (lists of path
;; components), e.g. ("app") and ("app" "ui" "button.js") => "./ui/button.js".
(define (relative-path from to)
  (let lp ((from from) (to to))
    (if (and (pair? from) (pair? (cdr to)) (string=? (car from) (car to)))
        (lp (cdr from) (cdr to))
        (string-append (if (null? from)
                           "./"
                           (string-concatenate (map (const "../") from)))
                       (string-join to "/")))))

;; The import path of the module NAME (in FILE) from the module being
;; compiled.
(define (import-path ctx name file)
  (if (module-context-source-imports? ctx)
      (relative-path (string-split (dirname (canonicalize-path (module-context-file ctx))) #\/)
                     (string-split (canonicalize-path file) #\/))
      (relative-path (drop-right (string-split (module-context-path ctx) #\/) 1)
                     (string-split (string-append (module-path name) ".js") #\/))))

(define (spec-module-name spec src)
  (match spec
    ((? module-name? name) name)
    (((? module-name? name) . _) name)
    (_ (module-error src "invalid module specification ~s" spec))))

;; The names imported by SPEC from a GuileScript module exporting EXPORTS:
;; ((EXPORTED-NAME . LOCAL-NAME) ...)
(define (spec-imports spec exports src)
  (let* ((name (spec-module-name spec src))
         (options (if (module-name? spec) '() (cdr spec))))
    (let lp ((options options) (select #f) (prefix #f))
      (match options
        (()
         (let ((imports (if select
                            (map (match-lambda
                                   ((? symbol? s) (cons s s))
                                   (((? symbol? orig) . (? symbol? local)) (cons orig local))
                                   (other (module-error src "invalid #:select entry ~s" other)))
                                 select)
                            (map (lambda (s) (cons s s)) exports))))
           (for-each (match-lambda
                       ((orig . _)
                        (unless (memq orig exports)
                          (module-error src "`~a' is not exported by ~s" orig name))))
                     imports)
           (map (match-lambda
                  ((orig . local)
                   (cons orig (if prefix (symbol-append prefix local) local))))
                imports)))
        ((#:select (items ...) . rest) (lp rest items prefix))
        ((#:prefix (? symbol? p) . rest) (lp rest select p))
        ((#:version _ . rest) (lp rest select prefix))
        ((option . _)
         (module-error src "~s is not supported when importing GuileScript modules" option))))))

;; Forms importing the modules in SPECS.
(define (import-forms ctx specs src)
  (let-values (((gs-specs guile-specs)
                (partition (lambda (spec) (module-file ctx (spec-module-name spec src)))
                           specs)))
    (append
     (if (null? guile-specs) '() `((use-modules ,@guile-specs)))
     (map (lambda (spec)
            (let* ((name (spec-module-name spec src))
                   (file (module-file ctx name)))
              (unless (assoc name (%module-context-dependencies ctx))
                (set-module-context-dependencies!
                 ctx (cons (cons name file) (%module-context-dependencies ctx))))
              `(js-import ,(import-path ctx name file)
                          #:rename ',(spec-imports spec (module-exports file) src))))
          gs-specs))))

(define (transform-define-module ctx name options src)
  (set-module-name! ctx name src)
  (let lp ((options options) (specs '()) (exports '()))
    (match options
      (()
       `(begin
          ,@(import-forms ctx (reverse specs) src)
          ,@(if (null? exports) '() `((export ,@(reverse exports))))))
      ((#:use-module spec . rest)
       (lp rest (cons spec specs) exports))
      (((or #:export #:re-export #:replace) ((? symbol? names) ...) . rest)
       (lp rest specs (append (reverse names) exports)))
      (((or #:pure #:no-backtrace) . rest)
       (lp rest specs exports))
      (((or #:declarative? #:duplicates #:version) _ . rest)
       (lp rest specs exports))
      ((option . _)
       (module-error src "define-module option ~s is not supported" option)))))

;; The (begin form ...) FORM as a syntax object with the source location of
;; X (also in its forms, which become top-level forms).
(define (with-source x form)
  (let ((src (syntax-source x)))
    (match form
      (('begin forms ...)
       (datum->syntax x
                      (cons 'begin (map (lambda (f) (datum->syntax x f #:source src)) forms))
                      #:source src)))))

;; Rewrite a top-level form read from a GuileScript file (see Commentary).
;; X is a syntax object.
(define (transform-module-form x)
  (let ((ctx (current-module-context)))
    (if (or (not ctx) (eof-object? x))
        x
        (match (syntax->datum x)
          (('define-module (? module-name? name) . options)
           (with-source x (transform-define-module ctx name options (syntax-source x))))
          (('use-modules specs ...)
           (if (any (lambda (spec) (module-file ctx (spec-module-name spec (syntax-source x))))
                    specs)
               (with-source x `(begin ,@(import-forms ctx specs (syntax-source x))))
               x))
          (_ x)))))
