;;; (language javascript decompile-tree-il) --- GuileScript.

;; Copyright (C) 2022 Aleix Conchillo Flaque <aconchillo@gmail.com>
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

;; Tree-IL to JavaScript decompiler

;;; Code:

(define-module (language javascript decompile-tree-il)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 vlist)
  #:use-module (language tree-il)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-43)
  #:use-module (system base compile)
  #:use-module (system base syntax)
  #:export (decompile-tree-il))

(define (decompile-tree-il e env opts)
  (apply do-decompile e env opts))

(define* (do-decompile e env
                       #:key
                       (use-derived-syntax? #t)
                       (use-case? #t)
                       (strip-numeric-suffixes? #f)
                       #:allow-other-keys)

  (receive (output-name-table occurrence-count-table)
      (choose-output-names e use-derived-syntax? strip-numeric-suffixes?)

    (define (binary-operator? op)
      (member op '(< > <= >=)))

    (define (arithmetic-operator? op)
      (member op '(+ - * /)))

    (define (translate-identity proc args port)
      (put-string port (symbol->string proc))
      (put-string port "(")
      (put-string
       port
       (string-join
        (map
         (lambda (arg)
           (call-with-output-string (lambda (p) (recurse arg 0 p))))
         args)
        ","))
      (put-string port ")"))

    (define (translate-arithmetic-operator op args port)
      (put-string port "(")
      (put-string
       port
       (string-join
        (map (lambda (arg) (call-with-output-string (lambda (p) (recurse arg 0 p)))) args)
        (symbol->string op)))
      (put-string port ")"))

    (define (translate-binary-operator op args port)
      (put-string port (call-with-output-string (lambda (p) (recurse (first args) 0 p))))
      (put-string port (symbol->string op))
      (put-string port (call-with-output-string (lambda (p) (recurse (second args) 0 p)))))

    (define (translate-vector-length args port)
      (format port "~a.length" (call-with-output-string (lambda (p) (recurse (first args) 0 p)))))

    (define (translate-vector-ref args port)
      (format port "~a[~a]"
              (call-with-output-string (lambda (p) (recurse (first args) 0 p)))
              (call-with-output-string (lambda (p) (recurse (second args) 0 p)))))

    (define (translate-vector-set! args port)
      (format port "~a[~a] = ~a"
              (call-with-output-string (lambda (p) (recurse (first args) 0 p)))
              (call-with-output-string (lambda (p) (recurse (second args) 0 p)))
              (call-with-output-string (lambda (p) (recurse (third args) 0 p)))))

    ;; TODO: Probably use a hash table for constant access, but for now this is
    ;; fine. We could even check for right number of arguments, etc.
    (define (translate-function proc args port)
      (let ((op (or (output-name (toplevel-ref-name proc))
                    (toplevel-ref-name proc))))
        (match op
          ;; Logging
          ('log:info (translate-identity 'console.log args port))
          ('log:error (translate-identity 'console.error args port))
          ('log:warn (translate-identity 'console.warn args port))
          ;; Math
          ('abs (translate-identity 'Math.abs args port))
          ('ceiling (translate-identity 'Math.ceil args port))
          ('floor (translate-identity 'Math.floor args port))
          ('max (translate-identity 'Math.max args port))
          ('min (translate-identity 'Math.min args port
                 ))
          ('round (translate-identity 'Math.round args port))
          ;; Strings
          ('string-length (translate-vector-length args port))
          ('string-ref (translate-vector-ref args port))
          ;; Vectors
          ('vector-length (translate-vector-length args port))
          ('vector-ref (translate-vector-ref args port))
          ('vector-set! (translate-vector-set! args port))
          ;; Operators
          ('equal? (translate-binary-operator '=== args port))
          ((? binary-operator?) (translate-binary-operator op args port))
          ((? arithmetic-operator?) (translate-arithmetic-operator op args port))
          ;; Anything else
          (_ (translate-identity op args port)))))

    (define (build-indent-string port level)
      (put-string port (format #f "~v_" (* 2 level))))

    (define (output-name s)
      (hashq-ref output-name-table s))

    (define (wrap-with-return body indent port)
      (unless (seq? body)
        (build-indent-string port indent)
        (put-string port "return "))
      (recurse body indent port)
      (unless (seq? body)
        (put-string port ";\n")))

    (define (build-vector exp port)
      (put-string port "[")
      (vector-for-each
       (lambda (i v)
         (build-const v port)
         (when (< i (- (vector-length exp) 1))
           (put-char port #\,)))
       exp)
      (put-string port "]"))

    (define (build-const exp port)
      (cond
       ((nil? exp) (put-string port "null"))
       ((number? exp) (format port "~a" (if (integer? exp) exp (exact->inexact exp))))
       ((string? exp) (format port "\"~a\"" exp))
       ((boolean? exp) (put-string port (if exp "true" "false")))
       ((vector? exp) (build-vector exp port))))

    (define (build-call proc args indent port)
      (translate-function proc args port))

    (define (build-define name exp indent port)
      (format port "var ~a = " (symbol->string name))
      (recurse exp indent port))

    (define (build-set name exp indent port)
      (format port "~a = " (symbol->string name))
      (recurse exp indent port))

    (define (build-function meta body indent port)
      (let ((name (assoc-ref meta 'name)))
        (format port "function ~a" name)
        (recurse body indent port)))

    (define (build-anonymous-function meta body indent port)
      (put-string port "(function")
      (recurse body indent port)
      (put-string port ")"))

    (define (build-lambda meta body indent port)
      (if (assoc-ref meta 'name)
          (build-function meta body indent port)
          (build-anonymous-function meta body indent port)))

    (define (build-lambda-case req opt rest kw inits gensyms body alternate indent port)
      (put-string port "(")
      (put-string port (string-join (map symbol->string req) ","))
      (put-string port ") {\n")
      (wrap-with-return body (+ indent 1) port)
      (build-indent-string port indent)
      (put-string port "}"))

    (define (build-if test consequent indent port)
      (put-string port "(function () {\n")
      (build-indent-string port (+ indent 1))
      (put-string port "if (")
      (recurse test 0 port)
      (put-string port ") {\n")
      (build-indent-string port (+ indent 2))
      (wrap-with-return consequent (+ indent 2) port)
      (build-indent-string port (+ indent 1))
      (put-string port "}\n")
      (build-indent-string port indent)
      (put-string port "})()"))

    (define (build-if-ternary test consequent alternate indent port)
      (put-string port "(")
      (recurse test indent port)
      (put-string port " ? ")
      (recurse consequent indent port)
      (put-string port " : ")
      (recurse alternate indent port)
      (put-string port ")"))

    (define (build-conditional test consequent alternate indent port)
      (if (void? alternate)
          (build-if test consequent indent port)
          (build-if-ternary test consequent alternate indent port)))

    (define (build-seq head tail indent port)
      (build-indent-string port indent)
      (recurse head indent port)
      (put-string port ";\n")
      (wrap-with-return tail indent port))

    (define (build-let vars vals body indent port)
      (put-string port "(function (){\n")
      (for-each
       (lambda (var val)
         (build-indent-string port (+ indent 1))
         (format port "var ~a = ~a;\n"
                 var (call-with-output-string (lambda (p) (recurse val 0 p)))))
       vars vals)
      (wrap-with-return body (+ indent 1) port)
      (build-indent-string port indent)
      (put-string port "})()"))

    (define (build-letrec in-order? vars vals body indent port)
      (put-string port "(function (){\n")
      (build-indent-string port (+ indent 1))
      (build-define (car vars) (car vals) (+ indent 1) port)
      (put-string port ";\n")
      (wrap-with-return body (+ indent 1) port)
      (build-indent-string port indent)
      (put-string port "})()"))

    (define (recurse e indent port)
      (record-case
       e

       ((<void>)
        (if #f #f))

       ((<const> exp)
        (build-const exp port))

       ((<lexical-ref> gensym)
        (put-string port (symbol->string (output-name gensym))))

       ((<lexical-set> gensym exp)
        (build-set (output-name gensym) exp indent port))

       ((<toplevel-ref> name)
        (put-string port (symbol->string name)))

       ((<toplevel-define> name exp)
        (build-define name exp indent port))

       ((<toplevel-set> name exp)
        (build-set name exp indent port))

       ((<lambda> meta body)
        (build-lambda meta body indent port))

       ((<lambda-case> req opt rest kw inits gensyms body alternate)
        (build-lambda-case req opt rest kw inits gensyms body alternate indent port))

       ((<let> gensyms vals body)
        (build-let (map output-name gensyms) vals body indent port))

       ((<letrec> in-order? gensyms vals body)
        (build-letrec in-order? (map output-name gensyms) vals body indent port))

       ((<call> proc args)
        (build-call proc args indent port))

       ((<seq> head tail)
        (build-seq head tail indent port))

       ((<conditional> test consequent alternate)
        (build-conditional test consequent alternate indent port))))

    (values (call-with-output-string (lambda (p) (recurse e 0 p) (put-string p "\n"))) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Algorithm for choosing better variable names
;; ============================================
;;
;; This was copied from:
;;
;;  https://git.savannah.gnu.org/cgit/guile.git/tree/module/language/scheme/decompile-tree-il.scm

;;
;; 'choose-output-names' analyzes the top-level form e, chooses good
;; variable names that are as close as possible to the source names,
;; and returns two values:
;;
;;  * a hash table mapping gensym to output name
;;  * a hash table mapping gensym to number of occurrences
;;
(define choose-output-names
  (let ()
    (define primitive?
      ;; This is a list of primitives that 'tree-il->scheme' assumes
      ;; will have the standard bindings when found in operator
      ;; position.
      (let* ((primitives '(if quote @ @@ set! define define*
                              begin let let* letrec letrec*
                              and or cond case
                              lambda lambda* case-lambda case-lambda*
                              apply call-with-values dynamic-wind
                              with-fluids fluid-ref fluid-set!
                              call-with-prompt abort memv eqv?))
             (table (make-hash-table (length primitives))))
        (for-each (cut hashq-set! table <> #t) primitives)
        (lambda (name) (hashq-ref table name))))

    ;; Repeatedly strip suffix of the form "-N", where N is a string
    ;; that could be produced by number->string given a positive
    ;; integer.  In other words, the first digit of N may not be 0.
    (define compute-base-name
      (let ((digits (string->char-set "0123456789")))
        (define (base-name-string str)
          (let ((i (string-skip-right str digits)))
            (if (and i (< (1+ i) (string-length str))
                     (eq? #\- (string-ref str i))
                     (not (eq? #\0 (string-ref str (1+ i)))))
                (base-name-string (substring str 0 i))
                str)))
        (lambda (sym)
          (string->symbol (base-name-string (symbol->string sym))))))

    ;; choose-output-names
    (lambda (e use-derived-syntax? strip-numeric-suffixes?)

      (define lexical-gensyms '())

      (define top-level-intern!
        (let ((table (make-hash-table)))
          (lambda (name)
            (let ((h (hashq-create-handle! table name #f)))
              (or (cdr h) (begin (set-cdr! h (cons 'top-level name))
                                 (cdr h)))))))
      (define (top-level? s) (pair? s))
      (define (top-level-name s) (cdr s))

      (define occurrence-count-table (make-hash-table))
      (define (occurrence-count s) (or (hashq-ref occurrence-count-table s) 0))
      (define (increment-occurrence-count! s)
        (let ((h (hashq-create-handle! occurrence-count-table s 0)))
          (if (zero? (cdr h))
              (set! lexical-gensyms (cons s lexical-gensyms)))
          (set-cdr! h (1+ (cdr h)))))

      (define base-name
        (let ((table (make-hash-table)))
          (lambda (name)
            (let ((h (hashq-create-handle! table name #f)))
              (or (cdr h) (begin (set-cdr! h (compute-base-name name))
                                 (cdr h)))))))

      (define source-name-table (make-hash-table))
      (define (set-source-name! s name)
        (if (not (top-level? s))
            (let ((name (if strip-numeric-suffixes?
                            (base-name name)
                            name)))
              (hashq-set! source-name-table s name))))
      (define (source-name s)
        (if (top-level? s)
            (top-level-name s)
            (hashq-ref source-name-table s)))

      (define conflict-table (make-hash-table))
      (define (conflicts s) (or (hashq-ref conflict-table s) '()))
      (define (add-conflict! a b)
        (define (add! a b)
          (if (not (top-level? a))
              (let ((h (hashq-create-handle! conflict-table a '())))
                (if (not (memq b (cdr h)))
                    (set-cdr! h (cons b (cdr h)))))))
        (add! a b)
        (add! b a))

      (let recurse-with-bindings ((e e) (bindings vlist-null))
        (let recurse ((e e))

          ;; We call this whenever we encounter a top-level ref or set
          (define (top-level name)
            (let ((bname (base-name name)))
              (let ((s (top-level-intern! name))
                    (conflicts (vhash-foldq* cons '() bname bindings)))
                (for-each (cut add-conflict! s <>) conflicts))))

          ;; We call this whenever we encounter a primitive reference.
          ;; We must also call it for every primitive that might be
          ;; inserted by 'tree-il->scheme'.  It is okay to call this
          ;; even when 'tree-il->scheme' will not insert the named
          ;; primitive; the worst that will happen is for a lexical
          ;; variable of the same name to be renamed unnecessarily.
          (define (primitive name) (top-level name))

          ;; We call this whenever we encounter a lexical ref or set.
          (define (lexical s)
            (increment-occurrence-count! s)
            (let ((conflicts
                   (take-while
                    (lambda (s*) (not (eq? s s*)))
                    (reverse! (vhash-foldq* cons
                                            '()
                                            (base-name (source-name s))
                                            bindings)))))
              (for-each (cut add-conflict! s <>) conflicts)))

          (record-case e
                       ((<void>)  (primitive 'if)) ; (if #f #f)
                       ((<const>) (primitive 'quote))

                       ((<call> proc args)
                        (if (lexical-ref? proc)
                            (let* ((gensym (lexical-ref-gensym proc))
                                   (name (source-name gensym)))
                              ;; If the operator position contains a bare variable
                              ;; reference with the same source name as a standard
                              ;; primitive, we must ensure that it will be given a
                              ;; different name, so that 'tree-il->scheme' will not
                              ;; misinterpret the resulting expression.
                              (if (primitive? name)
                                  (add-conflict! gensym (top-level-intern! name)))))
                        (recurse proc)
                        (for-each recurse args))

                       ((<primitive-ref> name) (primitive name))
                       ((<primcall> name args) (primitive name) (for-each recurse args))

                       ((<lexical-ref> gensym) (lexical gensym))
                       ((<lexical-set> gensym exp)
                        (primitive 'set!) (lexical gensym) (recurse exp))

                       ((<module-ref> public?) (primitive (if public? '@ '@@)))
                       ((<module-set> public? exp)
                        (primitive 'set!) (primitive (if public? '@ '@@)) (recurse exp))

                       ((<toplevel-ref> name) (top-level name))
                       ((<toplevel-set> name exp)
                        (primitive 'set!) (top-level name) (recurse exp))
                       ((<toplevel-define> name exp) (top-level name) (recurse exp))

                       ((<conditional> test consequent alternate)
                        (cond (use-derived-syntax?
                               (primitive 'and) (primitive 'or)
                               (primitive 'cond) (primitive 'case)
                               (primitive 'else) (primitive '=>)))
                        (primitive 'if)
                        (recurse test) (recurse consequent) (recurse alternate))

                       ((<seq> head tail)
                        (primitive 'begin) (recurse head) (recurse tail))

                       ((<lambda> body)
                        (if body (recurse body) (primitive 'case-lambda)))

                       ((<lambda-case> req opt rest kw inits gensyms body alternate)
                        (primitive 'lambda)
                        (cond ((or opt kw alternate)
                               (primitive 'lambda*)
                               (primitive 'case-lambda)
                               (primitive 'case-lambda*)))
                        (primitive 'let)
                        (if use-derived-syntax? (primitive 'let*))
                        (let* ((names (append req (or opt '()) (if rest (list rest) '())
                                              (map cadr (if kw (cdr kw) '()))))
                               (base-names (map base-name names))
                               (body-bindings
                                (fold vhash-consq bindings base-names gensyms)))
                          (for-each increment-occurrence-count! gensyms)
                          (for-each set-source-name! gensyms names)
                          (for-each recurse inits)
                          (recurse-with-bindings body body-bindings)
                          (if alternate (recurse alternate))))

                       ((<let> names gensyms vals body)
                        (primitive 'let)
                        (cond (use-derived-syntax? (primitive 'let*) (primitive 'or)))
                        (for-each increment-occurrence-count! gensyms)
                        (for-each set-source-name! gensyms names)
                        (for-each recurse vals)
                        (recurse-with-bindings
                         body (fold vhash-consq bindings (map base-name names) gensyms)))

                       ((<letrec> in-order? names gensyms vals body)
                        (primitive 'let)
                        (cond (use-derived-syntax? (primitive 'let*) (primitive 'or)))
                        (primitive (if in-order? 'letrec* 'letrec))
                        (for-each increment-occurrence-count! gensyms)
                        (for-each set-source-name! gensyms names)
                        (let* ((base-names (map base-name names))
                               (bindings (fold vhash-consq bindings base-names gensyms)))
                          (for-each (cut recurse-with-bindings <> bindings) vals)
                          (recurse-with-bindings body bindings)))

                       ((<fix> names gensyms vals body)
                        (primitive 'let)
                        (primitive 'letrec*)
                        (cond (use-derived-syntax? (primitive 'let*) (primitive 'or)))
                        (for-each increment-occurrence-count! gensyms)
                        (for-each set-source-name! gensyms names)
                        (let* ((base-names (map base-name names))
                               (bindings (fold vhash-consq bindings base-names gensyms)))
                          (for-each (cut recurse-with-bindings <> bindings) vals)
                          (recurse-with-bindings body bindings)))

                       ((<let-values> exp body)
                        (primitive 'call-with-values)
                        (recurse exp) (recurse body))

                       ((<prompt> tag body handler)
                        (primitive 'call-with-prompt)
                        (recurse tag) (recurse body) (recurse handler))

                       ((<abort> tag args tail)
                        (primitive 'apply)
                        (primitive 'abort)
                        (recurse tag) (for-each recurse args) (recurse tail)))))

      (let ()
        (define output-name-table (make-hash-table))
        (define (set-output-name! s name)
          (hashq-set! output-name-table s name))
        (define (output-name s)
          (if (top-level? s)
              (top-level-name s)
              (hashq-ref output-name-table s)))

        (define sorted-lexical-gensyms
          (sort-list lexical-gensyms
                     (lambda (a b) (> (occurrence-count a)
                                      (occurrence-count b)))))

        (for-each (lambda (s)
                    (set-output-name!
                     s
                     (let ((the-conflicts (conflicts s))
                           (the-source-name (source-name s)))
                       (define (not-yet-taken? name)
                         (not (any (lambda (s*)
                                     (and=> (output-name s*)
                                            (cut eq? name <>)))
                                   the-conflicts)))
                       (if (not-yet-taken? the-source-name)
                           the-source-name
                           (let ((prefix (string-append
                                          (symbol->string the-source-name)
                                          "-")))
                             (let loop ((i 1) (name the-source-name))
                               (if (not-yet-taken? name)
                                   name
                                   (loop (+ i 1)
                                         (string->symbol
                                          (string-append
                                           prefix
                                           (number->string i)))))))))))
                  sorted-lexical-gensyms)
        (values output-name-table occurrence-count-table)))))
