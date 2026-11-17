;;; (language javascript decompile-tree-il) --- GuileScript.

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

;; Tree-IL to JavaScript compiler.
;;
;; Tree-IL is compiled into a small JavaScript AST (see (language javascript
;; print)) which is then printed. Every Tree-IL expression is compiled
;; either as a statement, in a context that says what to do with its value,
;; or as a value (a JavaScript expression):
;;
;;   compile-stmt X CTX ACC   emits statements into the accumulator ACC
;;   compile-value X ACC      returns an expression, but might first emit
;;                            statements into ACC (e.g. variable bindings)
;;
;; Statement contexts:
;;
;;   return             the value is returned from the current function
;;   effect             the value is ignored
;;   (assign . NAME)    the value is assigned to the variable NAME
;;   (LOOP . CTX)       tail position of a loop body: calls to the loop
;;                      become "continue" and values are delivered to CTX
;;
;; Functions that only call themselves in tail position (including named
;; let loops) are compiled into while loops, so they don't grow the stack.

;;; Code:

(define-module (language javascript decompile-tree-il)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (language tree-il)
  #:use-module (language javascript errors)
  #:use-module (language javascript names)
  #:use-module (language javascript primitives)
  #:use-module (language javascript print)
  #:use-module (language javascript source-map)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:export (decompile-tree-il
            compile-program))

(define (decompile-tree-il x env opts)
  (let-values (((stmts locations) (compile-program x (and (module? env) env))))
    (values (match (current-mappings-receiver)
              (#f (print-js stmts))
              (receive-mappings
               (let-values (((js mappings) (print-js-with-locations stmts locations)))
                 (receive-mappings mappings)
                 js)))
            env)))

;;
;; Compilation unit state
;;

(define-record-type <unit>
  (make-unit env bindings defined assigned exports imported js-names
             lexical-assigned inline-loops emitted gensym-names scopes form-scope
             records value-refs aliases preferred-names renamed generated-names
             stmt-locations ref-counts)
  unit?
  (env unit-env)                        ; module where the code was expanded
  (bindings unit-bindings)              ; name -> Guile binding (memoized)
  (defined unit-defined)                ; name -> number of definitions
  (assigned unit-assigned)              ; name -> #t if set!
  (exports unit-exports set-unit-exports!)
  (imported unit-imported)              ; name -> #t
  (js-names unit-js-names)              ; JS name -> top-level name
  (lexical-assigned unit-lexical-assigned) ; gensym -> #t if set!
  (inline-loops unit-inline-loops)      ; letrec -> boolean (memoized)
  (emitted unit-emitted)                ; name -> #t once defined
  (gensym-names unit-gensym-names)      ; gensym -> JS name
  (scopes unit-scopes set-unit-scopes!) ; one table per function scope
  ;; Variables declared at the top level by the current top-level form. The
  ;; last table in scopes has all the variables declared at the top level.
  (form-scope unit-form-scope set-unit-form-scope!)
  ;; Record definitions: name or gensym -> the %js-record-* call.
  (records unit-records)
  ;; Names and gensyms used as values (not only called).
  (value-refs unit-value-refs)
  ;; Copies: (let ((q t)) ...) where t is only used there. The variable t
  ;; is named like q (preferred-names) and q is the same variable (aliases).
  (aliases unit-aliases)                ; q gensym -> t gensym
  (preferred-names unit-preferred-names) ; t gensym -> q name
  (renamed unit-renamed)                ; t gensym -> #t once bound
  ;; Top-level variables introduced by macros -> JavaScript name.
  (generated-names unit-generated-names)
  ;; Statement -> source location (for source maps).
  (stmt-locations unit-stmt-locations)
  ;; gensym -> number of references.
  (ref-counts unit-ref-counts))

(define current-unit (make-parameter #f))

(define (new-unit env)
  (make-unit env (make-hash-table)
             (make-hash-table) (make-hash-table) '() (make-hash-table)
             (make-hash-table) (make-hash-table) (make-hash-table)
             (make-hash-table) (make-hash-table) (list (make-hash-table))
             (make-hash-table) (make-hash-table) (make-hash-table)
             (make-hash-table) (make-hash-table) (make-hash-table)
             (make-hash-table) (make-hash-table) (make-hash-table)))

(define (user-defined? name)
  (let ((unit (current-unit)))
    (or (hashq-ref (unit-defined unit) name #f)
        (hashq-ref (unit-imported unit) name #f))))

(define (lexical-assigned? gensym)
  (hashq-ref (unit-lexical-assigned (current-unit)) gensym #f))

(define (binding-kind gensym)
  (if (lexical-assigned? gensym) "let" "const"))

;; A variable that is never used (e.g. the dummy variables Guile introduces
;; for expressions between definitions).
(define (unused? gensym)
  (and (zero? (hashq-ref (unit-ref-counts (current-unit)) gensym 0))
       (not (lexical-assigned? gensym))))

;;
;; Statement accumulators
;;

(define (new-acc) (list 'acc))
;; The source location of the statements being emitted: the location of the
;; innermost expression being compiled that comes from the user's code.
(define %statement-location (make-fluid #f))

(define (call-with-statement-location x thunk)
  (let ((src (tree-il-src x)))
    (if (user-location? src)
        (with-fluids ((%statement-location src)) (thunk))
        (thunk))))

(define (emit! acc stmt)
  (let ((src (fluid-ref %statement-location)))
    (when src
      (hashq-set! (unit-stmt-locations (current-unit)) stmt src)))
  (set-cdr! acc (cons stmt (cdr acc))))
(define (emit-all! acc stmts) (for-each (cut emit! acc <>) stmts))
;; The statements in ACC, without the ones that can't be reached.
(define (acc-stmts acc)
  (let lp ((stmts (reverse (cdr acc))) (out '()))
    (match stmts
      (() (reverse out))
      ((stmt . rest)
       (if (exits? stmt)
           (reverse (cons stmt out))
           (lp rest (cons stmt out)))))))
(define (acc-empty? acc) (null? (cdr acc)))

;;
;; Names
;;

;; The JavaScript name another GuileScript module exports NAME with.
(define (exported-js-name name)
  (let ((s (symbol->string name)))
    (if (js-protected-global? s) s (scheme->js-name name))))

(define (toplevel-js-name name)
  (let ((s (symbol->string name)))
    (cond
     ((js-protected-global? s) s)
     ((generated-name? name) (generated-js-name name))
     (else (scheme->js-name name)))))

;; Variables introduced by macros have names like t-14f049806a705a7e-16.
(define (strip-generated-suffix name)
  (let ((s (symbol->string name)))
    (match (string-match "^(.+?)(-[0-9a-f]{12,}(-[0-9a-f]+)*)$" s)
      (#f name)
      (m (string->symbol (match:substring m 1))))))

(define (generated-name? name)
  (not (eq? name (strip-generated-suffix name))))

;; Top-level variables introduced by macros get a readable unique name (e.g.
;; tmp, tmp_1), chosen once all the other top-level names are known.
(define (generated-js-name name)
  (let* ((unit (current-unit))
         (table (unit-generated-names unit)))
    (or (hashq-ref table name)
        (let* ((base (scheme->js-name (strip-generated-suffix name)))
               (toplevel-scope (last (unit-scopes unit)))
               (js (let lp ((i 0))
                     (let ((candidate (if (zero? i)
                                          base
                                          (string-append base "_" (number->string i)))))
                       (if (or (hash-ref (unit-js-names unit) candidate #f)
                               (hash-ref toplevel-scope candidate #f)
                               (js-reserved? candidate)
                               (js-protected-global? candidate))
                           (lp (+ i 1))
                           candidate)))))
          (hash-set! (unit-js-names unit) js name)
          (hashq-set! table name js)
          js))))

(define (toplevel-scope?)
  (null? (cdr (unit-scopes (current-unit)))))

(define (name-taken? candidate source)
  (let* ((unit (current-unit))
         (owner (hash-ref (unit-js-names unit) candidate #f))
         (scopes (unit-scopes unit)))
    (or (js-reserved? candidate)
        (js-protected-global? candidate)
        ;; Variables at the top level would clash with top-level
        ;; definitions. Inside functions they can only shadow top-level
        ;; variables with the same Scheme name.
        (and owner (or (toplevel-scope?) (not (eq? owner source))))
        (if (toplevel-scope?)
            (hash-ref (car scopes) candidate #f)
            ;; Functions can only see the top-level variables declared by
            ;; their own top-level form.
            (or (hash-ref (unit-form-scope unit) candidate #f)
                (any (cut hash-ref <> candidate #f) (drop-right scopes 1)))))))

;; Declare a new variable in the current function scope. SOURCE is the
;; Scheme name (a symbol) or a JavaScript base name (a string).
(define (declare-name! source)
  (let ((base (if (string? source)
                  source
                  (scheme->js-name (strip-generated-suffix source)))))
    (let lp ((i 0))
      (let ((candidate (if (zero? i)
                           base
                           (string-append base "_" (number->string i)))))
        (if (name-taken? candidate source)
            (lp (+ i 1))
            (let ((unit (current-unit)))
              (hash-set! (car (unit-scopes unit)) candidate #t)
              (when (toplevel-scope?)
                (hash-set! (unit-form-scope unit) candidate #t))
              candidate))))))

(define (fresh-name base)
  (declare-name! base))

(define (bind! gensym source)
  (let ((name (declare-name! source)))
    (set-gensym-name! gensym name)
    name))

;; Like bind!, but a variable that is only copied into another one gets the
;; name of the latter (see unit-aliases).
(define (bind-local! gensym source)
  (let ((preferred (hashq-ref (unit-preferred-names (current-unit)) gensym #f)))
    (when preferred
      (hashq-set! (unit-renamed (current-unit)) gensym #t))
    (bind! gensym (or preferred source))))

;; If GENSYM is a copy of a variable that already has its name, the JavaScript
;; name of that variable.
(define (alias-name gensym)
  (let* ((unit (current-unit))
         (original (hashq-ref (unit-aliases unit) gensym #f)))
    (and original
         (hashq-ref (unit-renamed unit) original #f)
         (gensym-name original))))

(define (set-gensym-name! gensym name)
  (hashq-set! (unit-gensym-names (current-unit)) gensym name))

(define (gensym-name gensym)
  (or (hashq-ref (unit-gensym-names (current-unit)) gensym #f)
      (compile-error "internal error: unbound variable ~a" gensym)))

(define (call-with-function-scope thunk)
  (let* ((unit (current-unit))
         (scopes (unit-scopes unit)))
    (set-unit-scopes! unit (cons (make-hash-table) scopes))
    (call-with-values thunk
      (lambda results
        (set-unit-scopes! unit scopes)
        (apply values results)))))

;;
;; Tree-IL helpers
;;

(define (toplevel-forms x)
  (if (seq? x)
      (append (toplevel-forms (seq-head x)) (toplevel-forms (seq-tail x)))
      (list x)))

(define (tree-any pred x)
  (tree-il-fold (lambda (y found) (or found (pred y)))
                (lambda (y found) found)
                #f x))

(define (references-any? x gensyms)
  (tree-any (lambda (y)
              (or (and (lexical-ref? y) (memq (lexical-ref-gensym y) gensyms))
                  (and (lexical-set? y) (memq (lexical-set-gensym y) gensyms))))
            x))

(define (lexical-ref-to? x gensym)
  (and (lexical-ref? x) (eq? (lexical-ref-gensym x) gensym)))

;; A reference to the Guile procedure NAME (not redefined by the user).
(define (named-ref? x name)
  (or (and (toplevel-ref? x)
           (eq? (toplevel-ref-name x) name)
           (not (user-defined? name)))
      (and (module-ref? x) (eq? (module-ref-name x) name))))

(define (macro-definition? exp)
  (and (primcall? exp) (eq? (primcall-name exp) 'make-syntax-transformer)))

(define (keyword-const? x)
  (and (const? x) (keyword? (const-exp x))))

;; (export a b) and define-public expand into:
;;
;;   (call-with-deferred-observers
;;     (lambda () (module-export! (current-module) '(a b))))
(define (export-form-names x)
  (and (call? x)
       (named-ref? (call-proc x) 'call-with-deferred-observers)
       (match (call-args x)
         (((? lambda? thunk))
          (let ((clause (lambda-body thunk)))
            (and (lambda-case? clause)
                 (let ((body (lambda-case-body clause)))
                   (and (call? body)
                        (named-ref? (call-proc body) 'module-export!)
                        (match (call-args body)
                          ((_ (? const? names)) (const-exp names))
                          (_ #f)))))))
         (_ #f))))

(define (js-import-form? x)
  (and (call? x)
       (toplevel-ref? (call-proc x))
       (eq? (toplevel-ref-name (call-proc x)) 'js-import)))

;; (js-global name ...) declares JavaScript globals.
(define (js-global-form? x)
  (and (call? x)
       (toplevel-ref? (call-proc x))
       (eq? (toplevel-ref-name (call-proc x)) 'js-global)))

(define (global-form-names x)
  (map (lambda (arg)
         (unless (toplevel-ref? arg)
           (note-location! (tree-il-src x))
           (compile-error "js-global expects names, e.g. (js-global document window)"))
         (toplevel-ref-name arg))
       (call-args x)))

;; Calls that throw: (error msg irritant ...), (raise obj).
(define (throw-call? x)
  (and (call? x)
       (or (named-ref? (call-proc x) 'error)
           (named-ref? (call-proc x) 'throw)
           (named-ref? (call-proc x) 'raise)
           (named-ref? (call-proc x) 'raise-exception))))

;; guard expands into (%js-try (lambda () body ...) (lambda (e) handler ...)),
;; see (language guilescript prelude).
(define (try-call? x)
  (and (call? x) (named-ref? (call-proc x) '%js-try)))

;; The bodies of the functions given to %js-try, which are inlined.
(define (try-bodies x)
  (filter-map (lambda (f)
                (and (lambda? f)
                     (lambda-case? (lambda-body f))
                     (lambda-case-body (lambda-body f))))
              (call-args x)))

;; define-record-type expands into definitions of calls to %js-record-type,
;; %js-record-constructor, etc. (see (language guilescript prelude)).
(define (record-call-kind x)
  (and (call? x)
       (let ((proc (call-proc x)))
         (any (match-lambda
                ((name . kind) (and (named-ref? proc name) kind)))
              '((%js-record-type . type)
                (%js-record-constructor . constructor)
                (%js-record-predicate . predicate)
                (%js-record-accessor . accessor)
                (%js-record-modifier . modifier))))))

(define (toplevel-mutable? name)
  (let ((unit (current-unit)))
    (or (> (hashq-ref (unit-defined unit) name 0) 1)
        (hashq-ref (unit-assigned unit) name #f))))

(define (value-ref? key)
  (hashq-ref (unit-value-refs (current-unit)) key #f))

;; The record procedure definition (a %js-record-* call) PROC refers to, if
;; calls to it can be inlined.
(define (record-procedure-for proc)
  (let* ((records (unit-records (current-unit)))
         (def (cond
               ((toplevel-ref? proc)
                (and (not (toplevel-mutable? (toplevel-ref-name proc)))
                     (hashq-ref records (toplevel-ref-name proc) #f)))
               ((lexical-ref? proc)
                (and (not (lexical-assigned? (lexical-ref-gensym proc)))
                     (hashq-ref records (lexical-ref-gensym proc) #f)))
               (else #f))))
    (and def (not (eq? (record-call-kind def) 'type)) def)))

;; (call-with-values producer consumer). receive, let-values, etc. expand
;; into it.
(define (call-with-values? x)
  (and (call? x)
       (named-ref? (call-proc x) 'call-with-values)
       (= 2 (length (call-args x)))))

;; The clause of a lambda with only required arguments and an optional rest
;; argument, or #f.
(define (values-clause x)
  (and (lambda? x)
       (let ((clause (lambda-body x)))
         (and (lambda-case? clause)
              (not (lambda-case-alternate clause))
              (not (lambda-case-opt clause))
              (not (lambda-case-kw clause))
              clause))))

;; The body of PRODUCER if it's a thunk literal, or #f.
(define (thunk-body producer)
  (let ((clause (values-clause producer)))
    (and clause
         (null? (lambda-case-req clause))
         (not (lambda-case-rest clause))
         (lambda-case-body clause))))

;; The parts of a call-with-values that are compiled inline: the producer
;; and consumer bodies (if they are literal lambdas).
(define (call-with-values-bodies x)
  (match (call-args x)
    ((producer consumer)
     (append (if (thunk-body producer) (list (thunk-body producer)) (list producer))
             (if (values-clause consumer)
                 (list (lambda-case-body (values-clause consumer)))
                 (list consumer))))))

;; (use-modules ...) expands into a call to process-use-modules.
(define (use-modules-form? x)
  (and (call? x) (named-ref? (call-proc x) 'process-use-modules)))

(define (define-module-form? x)
  (tree-any (lambda (y) (and (call? y) (named-ref? (call-proc y) 'define-module*)))
            x))

;;
;; Guile bindings
;;
;; Names are looked up in the module where the code was expanded, to know if
;; they come from Guile or from a module imported with use-modules. Macros
;; from any module are fine (they are expanded at compile time), but
;; procedures are only supported if GuileScript compiles them inline.

;; The Guile binding of NAME: (MODULE-NAME ORIGINAL-NAME VARIABLE), or #f if
;; NAME is not bound in Guile (so it is a JavaScript global). ORIGINAL-NAME is
;; the name in MODULE-NAME (it can be different with #:select or #:prefix).
(define (guile-binding name)
  (let ((cache (unit-bindings (current-unit))))
    (match (hashq-ref cache name 'unknown)
      ('unknown
       (let ((binding (lookup-guile-binding (unit-env (current-unit)) name)))
         (hashq-set! cache name binding)
         binding))
      (binding binding))))

(define (lookup-guile-binding env name)
  (if env
      ;; The variable NAME resolves to, and the imported module providing it
      ;; (possibly re-exported). Imported modules can replace bindings from
      ;; Guile core (e.g. format from (ice-9 format)), so (guile) is the
      ;; provider only if no other module provides the same variable.
      (let* ((var (module-variable env name))
             (providers (if var
                            (filter (lambda (iface) (eq? var (module-variable iface name)))
                                    (module-uses env))
                            '()))
             (iface (or (find (lambda (iface) (not (equal? (module-name iface) '(guile))))
                              providers)
                        (and (pair? providers) (car providers)))))
        (and iface
             (let ((module (module-name iface)))
               (list module
                     (if (eq? (module-kind iface) 'custom-interface)
                         (or (original-name module var) name)
                         name)
                     var))))
      (let ((var (module-variable the-root-module name)))
        (and var (list '(guile) name var)))))

(define (original-name module var)
  (call/ec
   (lambda (return)
     (module-for-each (lambda (name v) (when (eq? v var) (return name)))
                      (resolve-interface module))
     #f)))

;; Primitives are the standard Guile procedures, so a binding is a primitive
;; only if it's the same variable as the one in (guile) or (srfi srfi-1).
(define (standard-variable? name var)
  (any (lambda (module)
         (eq? var (module-variable (resolve-interface module) name)))
       '((guile) (srfi srfi-1))))

(define (primitive-for x)
  (cond
   ((toplevel-ref? x)
    (let ((name (toplevel-ref-name x)))
      (and (not (user-defined? name))
           (match (guile-binding name)
             (#f (lookup-primitive name))
             ((module original var)
              (and (standard-variable? original var)
                   (lookup-primitive original)))))))
   ((module-ref? x) (lookup-primitive (module-ref-name x)))
   ((primitive-ref? x) (lookup-primitive (primitive-ref-name x)))
   (else #f)))

;; (.method obj arg ...): returns the JavaScript method name.
(define (method-ref x)
  (and (toplevel-ref? x)
       (method-symbol? (toplevel-ref-name x))
       (not (user-defined? (toplevel-ref-name x)))
       (method-js-name (toplevel-ref-name x))))

;; (.-prop obj): returns the JavaScript property name.
(define (property-ref x)
  (and (toplevel-ref? x)
       (property-symbol? (toplevel-ref-name x))
       (not (user-defined? (toplevel-ref-name x)))
       (property-js-name (toplevel-ref-name x))))

;; (set! (f arg ...) value) expands to ((setter f) arg ... value). Returns f.
(define (setter-target proc)
  (and (call? proc)
       (named-ref? (call-proc proc) 'setter)
       (match (call-args proc)
         ((target) target)
         (_ #f))))

(define %special-procedures
  '(apply error throw raise raise-exception js-import js-global js-await
          call-with-values))

;; Does X use js-await, not counting nested functions (which would be async
;; themselves)? Inlined loops are not functions.
(define (contains-await? x)
  (let walk ((x x))
    (cond
     ((and (call? x) (named-ref? (call-proc x) 'js-await)) #t)
     ((try-call? x) (any walk (try-bodies x)))
     ((call-with-values? x) (any walk (call-with-values-bodies x)))
     ((lambda? x) #f)
     ((and (letrec? x) (inline-loop? x))
      (or (any walk (call-args (letrec-body x)))
          (walk (lambda-case-body (lambda-body (car (letrec-vals x)))))))
     (else (any walk (children x))))))

;; A function using js-await is compiled into an async function.
(define (async-lambda? x)
  (any contains-await? (children x)))

;;
;; Analysis
;;

;; The names imported by js-import, including the local names of
;; #:rename '((exported . local) ...).
(define (import-form-names x)
  (append-map (lambda (arg)
                (cond
                 ((toplevel-ref? arg) (list (toplevel-ref-name arg)))
                 ((and (const? arg) (list? (const-exp arg)))
                  (filter-map (lambda (entry) (and (pair? entry) (cdr entry)))
                              (const-exp arg)))
                 (else '())))
              (call-args x)))

(define (analyze! x)
  (let ((unit (current-unit)))
    (for-each
     (lambda (form)
       (cond
        ((toplevel-define? form)
         (unless (macro-definition? (toplevel-define-exp form))
           (let ((name (toplevel-define-name form)))
             (hashq-set! (unit-defined unit) name
                         (+ 1 (hashq-ref (unit-defined unit) name 0))))))
        ((js-import-form? form)
         (for-each (cut hashq-set! (unit-imported unit) <> #t)
                   (import-form-names form)))
        ((js-global-form? form)
         (for-each (cut hashq-set! (unit-imported unit) <> #t)
                   (global-form-names form)))))
     (toplevel-forms x))
    (for-each
     (lambda (form)
       (let ((names (export-form-names form)))
         (when names
           (set-unit-exports! unit (append (unit-exports unit) names)))))
     (toplevel-forms x))
    ;; Assignments and top-level names (macro definitions are only used at
    ;; compile time, so they are skipped).
    (for-each
     (lambda (form)
       (unless (and (toplevel-define? form) (macro-definition? (toplevel-define-exp form)))
         (analyze-form! form)))
     (toplevel-forms x))
    (for-each (lambda (name)
                (unless (user-defined? name)
                  (compile-error "exported name `~a' is not defined" name)))
              (unit-exports unit))))

(define (analyze-form! x)
  (let ((unit (current-unit))
        (called (make-hash-table))
        (ref-counts (unit-ref-counts (current-unit)))
        (copies '()))
    ;; References in call position.
    (tree-il-fold (lambda (y seed)
                    (when (call? y) (hashq-set! called (call-proc y) #t))
                    seed)
                  (lambda (y seed) seed)
                  #f x)
    (tree-il-fold
     (lambda (y seed)
       (cond
        ((hashq-ref called y) #t)
        ((toplevel-ref? y) (hashq-set! (unit-value-refs unit) (toplevel-ref-name y) #t))
        ((lexical-ref? y) (hashq-set! (unit-value-refs unit) (lexical-ref-gensym y) #t)))
       (cond
        ((and (toplevel-define? y) (record-call-kind (toplevel-define-exp y)))
         (hashq-set! (unit-records unit) (toplevel-define-name y) (toplevel-define-exp y)))
        ((letrec? y)
         (for-each (lambda (gensym val)
                     (when (record-call-kind val)
                       (hashq-set! (unit-records unit) gensym val)))
                   (letrec-gensyms y) (letrec-vals y)))
        ((lexical-ref? y)
         (hashq-set! ref-counts (lexical-ref-gensym y)
                     (+ 1 (hashq-ref ref-counts (lexical-ref-gensym y) 0))))
        ((let? y)
         (for-each (lambda (name gensym val)
                     (when (lexical-ref? val)
                       (set! copies (cons (list gensym name (lexical-ref-gensym val))
                                          copies))))
                   (let-names y) (let-gensyms y) (let-vals y))))
       (cond
        ((and (not (eq? y x)) (or (js-import-form? y) (js-global-form? y)))
         (note-location! (or (tree-il-src y) (first-user-location y)))
         (compile-error "~a is only allowed at the top level"
                        (toplevel-ref-name (call-proc y))))
        ((lexical-set? y)
         (hashq-set! (unit-lexical-assigned unit) (lexical-set-gensym y) #t))
        ((toplevel-set? y)
         (hashq-set! (unit-assigned unit) (toplevel-set-name y) #t)
         (register-js-name! (toplevel-set-name y)))
        ((toplevel-define? y)
         (unless (macro-definition? (toplevel-define-exp y))
           (register-js-name! (toplevel-define-name y))))
        ((toplevel-ref? y)
         (let ((name (toplevel-ref-name y)))
           (unless (or (primitive-for y)
                       (method-ref y)
                       (property-ref y)
                       (and (memq name %special-procedures)
                            (not (user-defined? name))))
             (check-guile-binding! name (tree-il-src y))
             (register-js-name! name)))))
       seed)
     (lambda (y seed) seed)
     #f x)
    (for-each (match-lambda
                ((gensym name original)
                 (when (and (= 1 (hashq-ref ref-counts original 0))
                            (not (lexical-assigned? original))
                            (not (lexical-assigned? gensym)))
                   (hashq-set! (unit-aliases unit) gensym original)
                   (hashq-set! (unit-preferred-names unit) original name))))
              copies)))

;; Every name used by the program must be defined, imported (js-import),
;; declared (js-global), a GuileScript built-in or a standard JavaScript
;; global, so mistakes are found at compile time.
(define (check-guile-binding! name src)
  (unless (user-defined? name)
    (match (guile-binding name)
      (#f
       (unless (js-standard-global? (toplevel-js-name name))
         (note-location! src)
         (compile-error "`~a' is not defined (if it's a JavaScript global, declare it with (js-global ~a))"
                        name name)))
      (('(guile) _ _)
       (note-location! src)
       (compile-error "`~a' is a Guile procedure not supported by GuileScript (if it's a JavaScript global, declare it with (js-global ~a))"
                      name name))
      ((module _ _)
       (note-location! src)
       (compile-error "`~a' from ~a is not supported by GuileScript (only macros and GuileScript built-in procedures can be imported from Guile modules)"
                      name module)))))

;; Two different Scheme names must not compile to the same JavaScript name.
;; (Names introduced by macros get their JavaScript name later.)
(define (register-js-name! name)
  (unless (generated-name? name)
    (register-user-js-name! name)))

(define (register-user-js-name! name)
  (let* ((table (unit-js-names (current-unit)))
         (js (toplevel-js-name name))
         (owner (hash-ref table js #f)))
    (cond
     ((not owner) (hash-set! table js name))
     ((not (eq? owner name))
      (compile-error "`~a' and `~a' both compile to the JavaScript name `~a'"
                     owner name js)))))

;;
;; Loops
;;
;; A target is (lexical . GENSYM) or (toplevel . NAME), the name a function
;; is bound to.

(define (target-ref? x target)
  (match target
    (('lexical . gensym) (lexical-ref-to? x gensym))
    (('toplevel . name) (and (toplevel-ref? x) (eq? (toplevel-ref-name x) name)))))

(define (target-mentioned? x target)
  (tree-any (lambda (y)
              (or (target-ref? y target)
                  (match target
                    (('lexical . gensym)
                     (and (lexical-set? y) (eq? (lexical-set-gensym y) gensym)))
                    (('toplevel . name)
                     (and (toplevel-set? y) (eq? (toplevel-set-name y) name))))))
            x))

;; Number of self tail calls to TARGET in BODY, or #f if TARGET is
;; referenced in any other way (e.g. non-tail calls, used as a value or
;; from a nested function).
(define (count-tail-calls body target nreq)
  (call/ec
   (lambda (fail)
     (define (no-refs! x)
       (when (target-mentioned? x target)
         (fail #f)))
     (let walk ((x body))
       (cond
        ((and (call-with-values? x) (values-clause (cadr (call-args x))))
         (no-refs! (car (call-args x)))
         (walk (lambda-case-body (values-clause (cadr (call-args x))))))
        ((call? x)
         (if (target-ref? (call-proc x) target)
             (begin
               (unless (= (length (call-args x)) nreq)
                 (fail #f))
               (for-each no-refs! (call-args x))
               1)
             (begin
               (no-refs! x)
               0)))
        ((conditional? x)
         (no-refs! (conditional-test x))
         (+ (walk (conditional-consequent x)) (walk (conditional-alternate x))))
        ((seq? x)
         (no-refs! (seq-head x))
         (walk (seq-tail x)))
        ((let? x)
         (for-each no-refs! (let-vals x))
         (walk (let-body x)))
        ((and (letrec? x) (inline-loop? x))
         ;; The body of an inlined loop is in tail position.
         (for-each no-refs! (call-args (letrec-body x)))
         (walk (lambda-case-body (lambda-body (car (letrec-vals x))))))
        ((letrec? x)
         (for-each no-refs! (letrec-vals x))
         (walk (letrec-body x)))
        (else
         (no-refs! x)
         0))))))

(define (simple-clause? clause)
  (and (lambda-case? clause)
       (not (lambda-case-alternate clause))
       (not (lambda-case-opt clause))
       (not (lambda-case-rest clause))
       (not (lambda-case-kw clause))))

;; A named let (or a single internal function only called right away) whose
;; function only calls itself in tail position:
;;
;;   (letrec ((loop (lambda (i ...) body))) (loop init ...))
(define (inline-loop? x)
  (let ((memo (unit-inline-loops (current-unit))))
    (match (hashq-ref memo x 'unknown)
      ('unknown
       (let ((result (compute-inline-loop? x)))
         (hashq-set! memo x result)
         result))
      (result result))))

(define (compute-inline-loop? x)
  (match (list (letrec-gensyms x) (letrec-vals x) (letrec-body x))
    (((gensym) ((? lambda? f)) (? call? body))
     (let ((clause (lambda-body f)))
       (and (simple-clause? clause)
            (lexical-ref-to? (call-proc body) gensym)
            (not (lexical-assigned? gensym))
            (= (length (call-args body)) (length (lambda-case-req clause)))
            (not (any (cut references-any? <> (list gensym)) (call-args body)))
            (count-tail-calls (lambda-case-body clause)
                              `(lexical . ,gensym)
                              (length (lambda-case-req clause)))
            #t)))
    (_ #f)))

;; The direct subexpressions of X.
(define (children x)
  (cond
   ((call? x) (cons (call-proc x) (call-args x)))
   ((primcall? x) (primcall-args x))
   ((conditional? x)
    (list (conditional-test x) (conditional-consequent x) (conditional-alternate x)))
   ((seq? x) (list (seq-head x) (seq-tail x)))
   ((let? x) (append (let-vals x) (list (let-body x))))
   ((letrec? x) (append (letrec-vals x) (list (letrec-body x))))
   ((fix? x) (append (fix-vals x) (list (fix-body x))))
   ((lambda? x) (if (lambda-body x) (list (lambda-body x)) '()))
   ((lambda-case? x)
    (append (lambda-case-inits x)
            (list (lambda-case-body x))
            (if (lambda-case-alternate x) (list (lambda-case-alternate x)) '())))
   ((lexical-set? x) (list (lexical-set-exp x)))
   ((toplevel-set? x) (list (toplevel-set-exp x)))
   ((toplevel-define? x) (list (toplevel-define-exp x)))
   ((module-set? x) (list (module-set-exp x)))
   ((let-values? x) (list (let-values-exp x) (let-values-body x)))
   ((prompt? x) (list (prompt-tag x) (prompt-body x) (prompt-handler x)))
   ((abort? x) (append (list (abort-tag x)) (abort-args x) (list (abort-tail x))))
   (else '())))

;; Does any function inside BODY capture one of GENSYMS? If so, loop
;; variables need a fresh binding on each iteration. Inlined loops are not
;; functions.
(define (captures-any? body gensyms)
  (let walk ((x body))
    (cond
     ((try-call? x) (any walk (try-bodies x)))
     ((call-with-values? x) (any walk (call-with-values-bodies x)))
     ((lambda? x) (references-any? x gensyms))
     ((and (letrec? x) (inline-loop? x))
      (or (any walk (call-args (letrec-body x)))
          (walk (lambda-case-body (lambda-body (car (letrec-vals x)))))))
     (else (any walk (children x))))))

(define-record-type <loop>
  (make-loop target vars label)
  loop?
  (target loop-target)
  (vars loop-vars)                      ; JavaScript loop variables
  (label loop-label))                   ; (NAME . USED?)

;;
;; Contexts
;;

(define (loop-ctx? ctx) (and (pair? ctx) (loop? (car ctx))))

(define (ctx-loops ctx)
  (if (loop-ctx? ctx) (cons (car ctx) (ctx-loops (cdr ctx))) '()))

(define (ctx-base ctx)
  (if (loop-ctx? ctx) (ctx-base (cdr ctx)) ctx))

;; break/continue LOOP from within CTX.
(define (loop-jump kind loop ctx)
  (if (eq? loop (car (ctx-loops ctx)))
      (list kind #f)
      (let ((label (loop-label loop)))
        (set-cdr! label #t)
        (list kind (car label)))))

(define (find-loop ctx proc)
  (find (lambda (loop) (target-ref? proc (loop-target loop)))
        (ctx-loops ctx)))

(define (pure-expr? e)
  (match e
    ((or ('num _) ('str _) ('bool _) ('null) ('undefined) ('id _)
         ('arrow . _) ('function . _) ('async . _))
     #t)
    (_ #f)))

;; Simple enough to be evaluated more than once or to be evaluated out of
;; order: literals, variables, functions and property accesses like
;; Math.max.
(define (simple-expr? e)
  (match e
    (('dot obj _) (simple-expr? obj))
    (_ (pure-expr? e))))

;; Deliver the value E (#f or (undefined) if unspecified) to CTX.
(define (deliver! ctx e acc)
  (let ((e (if (equal? e '(undefined)) #f e))
        (base (ctx-base ctx))
        (loops (ctx-loops ctx)))
    (match base
      ('return
       (cond
        (e (emit! acc `(return ,e)))
        ((pair? loops) (emit! acc '(return #f)))))
      ('effect
       (when (and e (not (pure-expr? e)))
         (emit! acc `(expr-stmt ,e))))
      (('assign . name)
       (when e
         (emit! acc `(expr-stmt (assign (id ,name) ,e))))))
    (when (and (pair? loops) (not (eq? base 'return)))
      (emit! acc (loop-jump 'break (last loops) ctx)))))

(define (hoist! e acc)
  (let ((t (fresh-name "t")))
    (emit! acc `(decl "const" ,t ,e))
    `(id ,t)))

;;
;; Programs
;;

(define* (compile-program x #:optional env)
  (parameterize ((current-unit (new-unit env)))
    (reset-location!)
    (analyze! x)
    (let ((acc (new-acc))
          (unit (current-unit)))
      (for-each (cut compile-toplevel <> acc) (toplevel-forms x))
      (let ((reexports (filter (lambda (name)
                                 (not (hashq-ref (unit-defined unit) name #f)))
                               (delete-duplicates (unit-exports unit)))))
        (unless (null? reexports)
          (emit! acc `(export-names ,(map toplevel-js-name reexports)))))
      (values (acc-stmts acc) (unit-stmt-locations unit)))))

;; The first location in the user's file found in X (macro expansions might
;; not have one at the top).
(define (first-user-location x)
  (tree-il-fold (lambda (y found)
                  (or found
                      (and (user-location? (tree-il-src y)) (tree-il-src y))))
                (lambda (y found) found)
                #f x))

(define (compile-toplevel x acc)
  (set-unit-form-scope! (current-unit) (make-hash-table))
  (note-location! (first-user-location x))
  (with-fluids ((%statement-location (first-user-location x)))
    (compile-toplevel* x acc)))

(define (compile-toplevel* x acc)
  (cond
   ((toplevel-define? x) (compile-definition x acc))
   ((use-modules-form? x) #t)
   ((js-global-form? x) #t)
   ((define-module-form? x)
    (compile-error "define-module is not supported yet, use use-modules to import Guile modules"))
   ((export-form-names x) #t)
   ((js-import-form? x) (emit! acc (compile-import x)))
   (else (compile-stmt x 'effect acc))))

(define (compile-definition x acc)
  (let* ((unit (current-unit))
         (name (toplevel-define-name x))
         (exp (toplevel-define-exp x))
         (js (toplevel-js-name name))
         (export? (memq name (unit-exports unit)))
         (mutable? (or (> (hashq-ref (unit-defined unit) name 0) 1)
                       (hashq-ref (unit-assigned unit) name #f)))
         (wrap (lambda (stmt) (if export? `(export ,stmt) stmt))))
    (cond
     ((macro-definition? exp) #t)
     ((hashq-ref (unit-emitted unit) name #f)
      ;; A redefinition is just an assignment.
      (compile-assignment! js exp acc))
     ((and (eq? (record-call-kind exp) 'type) (not mutable?))
      (hashq-set! (unit-emitted unit) name #t)
      (emit! acc (wrap (compile-record-class js exp))))
     ((and (record-call-kind exp) (not mutable?))
      ;; Calls to record procedures are inlined, so they are only defined
      ;; if they are exported or used as values.
      (hashq-set! (unit-emitted unit) name #t)
      (when (or export? (value-ref? name))
        (emit! acc (wrap (compile-record-function js exp acc)))))
     (else
      (hashq-set! (unit-emitted unit) name #t)
      (if (and (lambda? exp) (not mutable?))
          (emit! acc (wrap (compile-function-declaration js exp `(toplevel . ,name))))
          (compile-declaration! (if mutable? "let" "const") js exp acc wrap))))))

;; (js-import "module" [#:default Name] [#:as Name] name ...)
(define (compile-import x)
  (match (call-args x)
    (((? const? from) . args)
     (unless (string? (const-exp from))
       (compile-error "js-import expects a module name string"))
     (let lp ((args args) (default #f) (namespace #f) (names '()))
       (match args
         (()
          (when (and namespace (pair? names))
            (compile-error "js-import can't combine #:as with named imports"))
          `(import ,(const-exp from) ,default ,namespace ,(reverse names)))
         ;; #:rename '((exported . local) ...): import { exported as local }
         (((? keyword-const? kw) (? const? renames) . rest)
          (=> next)
          (if (eq? (const-exp kw) #:rename)
              (lp rest default namespace
                  (append (reverse
                           (map (match-lambda
                                  (((? symbol? exported) . (? symbol? local))
                                   (let ((exported (exported-js-name exported))
                                         (local (toplevel-js-name local)))
                                     (if (string=? exported local)
                                         local
                                         (string-append exported " as " local))))
                                  (other (compile-error "invalid js-import #:rename entry ~s" other)))
                                (const-exp renames)))
                          names))
              (next)))
         (((? keyword-const? kw) (? toplevel-ref? ref) . rest)
          (let ((js (toplevel-js-name (toplevel-ref-name ref))))
            (match (const-exp kw)
              (#:default (lp rest js namespace names))
              (#:as (lp rest default js names))
              (other (compile-error "unknown js-import option ~s" other)))))
         (((? toplevel-ref? ref) . rest)
          (lp rest default namespace
              (cons (toplevel-js-name (toplevel-ref-name ref)) names)))
         (_ (compile-error "invalid js-import form")))))
    (_ (compile-error "js-import expects a module name string"))))

;;
;; Bindings
;;

(define* (compile-declaration! kind name x acc #:optional (wrap identity))
  (if (expression-like? x)
      (let ((e (compile-value x acc)))
        (emit! acc (wrap `(decl ,kind ,name ,e))))
      (begin
        (emit! acc (wrap `(decl "let" ,name #f)))
        (compile-stmt x `(assign . ,name) acc))))

(define (compile-assignment! name x acc)
  (if (expression-like? x)
      (emit! acc `(expr-stmt (assign (id ,name) ,(compile-value x acc))))
      (compile-stmt x `(assign . ,name) acc)))

(define (compile-let-bindings! x acc)
  (for-each (lambda (gensym name val)
              (cond
               ((unused? gensym) (compile-stmt val 'effect acc))
               ((alias-name gensym) => (cut set-gensym-name! gensym <>))
               (else
                (compile-declaration! (binding-kind gensym) (bind-local! gensym name) val acc))))
            (let-gensyms x) (let-names x) (let-vals x)))

(define (compile-letrec-bindings! x acc)
  (let* ((gensyms (letrec-gensyms x))
         (names (map-in-order bind! gensyms (letrec-names x))))
    (for-each (lambda (gensym name val)
                (cond
                 ((and (lambda? val) (not (lexical-assigned? gensym)))
                  (emit! acc (compile-function-declaration name val `(lexical . ,gensym))))
                 ((and (eq? (record-call-kind val) 'type) (not (lexical-assigned? gensym)))
                  (emit! acc (compile-record-class name val)))
                 ((and (record-call-kind val) (not (lexical-assigned? gensym)))
                  (when (value-ref? gensym)
                    (emit! acc (compile-record-function name val acc))))
                 ((unused? gensym) (compile-stmt val 'effect acc))
                 (else
                  (compile-declaration! (binding-kind gensym) name val acc))))
              gensyms names (letrec-vals x))))

;;
;; Records
;;

(define (field-js-name field)
  (scheme->js-name field #:property? #t))

;; (%js-record-type 'type '(constructor-field ...) '(field ...)) =>
;;
;;   class Type {
;;     constructor(constructor-field, ...) { this.field = ...; ... }
;;   }
(define (compile-record-class name def)
  (match (call-args def)
    ((_ (? const? constructor-fields) (? const? fields))
     (call-with-function-scope
      (lambda ()
        (let ((params (map-in-order (lambda (field) (cons field (declare-name! field)))
                                    (const-exp constructor-fields))))
          `(class ,name
             ((constructor
               ,(map (lambda (p) `(param ,(cdr p))) params)
               ,(map (lambda (field)
                       `(expr-stmt
                         (assign (dot (this) ,(field-js-name field))
                                 ,(match (assq field params)
                                    ((_ . param) `(id ,param))
                                    (#f '(undefined))))))
                     (const-exp fields)))))))))
    (_ (compile-error "invalid record type definition"))))

;; A call to the record procedure defined by DEF (a %js-record-* call).
(define (compile-record-call def args acc)
  (define (check-arity n)
    (unless (= n (length args))
      (compile-error "wrong number of arguments to a record procedure")))
  (match (cons (record-call-kind def) (call-args def))
    (('constructor type (? const? fields))
     (check-arity (length (const-exp fields)))
     (match (compile-exprs (cons type args) acc)
       ((type . exprs) `(new ,type ,@exprs))))
    (('predicate type)
     (check-arity 1)
     (match (compile-exprs (cons type args) acc)
       ((type obj) `(binop "instanceof" ,obj ,type))))
    (('accessor _ (? const? field))
     (check-arity 1)
     `(dot ,(compile-value (car args) acc) ,(field-js-name (const-exp field))))
    (('modifier _ (? const? field))
     (check-arity 2)
     (match (compile-exprs args acc)
       ((obj value) `(assign (dot ,obj ,(field-js-name (const-exp field))) ,value))))
    (_ (compile-error "invalid record definition"))))

;; The record procedure defined by DEF as a function: (values PARAMS BODY).
(define (record-function def acc)
  (let ((type (compile-value (car (call-args def)) acc)))
    (call-with-function-scope
     (lambda ()
       (match (cons (record-call-kind def) (call-args def))
         (('constructor _ (? const? fields))
          (let ((params (map-in-order declare-name! (const-exp fields))))
            (values (map (lambda (p) `(param ,p)) params)
                    `((return (new ,type ,@(map (lambda (p) `(id ,p)) params)))))))
         (('predicate _)
          (let ((obj (fresh-name "obj")))
            (values `((param ,obj)) `((return (binop "instanceof" (id ,obj) ,type))))))
         (('accessor _ (? const? field))
          (let ((obj (fresh-name "obj")))
            (values `((param ,obj))
                    `((return (dot (id ,obj) ,(field-js-name (const-exp field))))))))
         (('modifier _ (? const? field))
          (let ((obj (fresh-name "obj"))
                (value (fresh-name "value")))
            (values `((param ,obj) (param ,value))
                    `((expr-stmt (assign (dot (id ,obj) ,(field-js-name (const-exp field)))
                                         (id ,value)))))))
         (_ (compile-error "invalid record definition")))))))

(define (compile-record-function name def acc)
  (let-values (((params body) (record-function def acc)))
    `(fun-decl ,name ,params ,body)))

;; (let ((t A)) (if t t B)) is what (or A B) expands to.
(define (or-pattern x)
  (and (let? x)
       (match (list (let-gensyms x) (let-vals x) (let-body x))
         (((gensym) (a) (? conditional? body))
          (and (lexical-ref-to? (conditional-test body) gensym)
               (lexical-ref-to? (conditional-consequent body) gensym)
               (not (references-any? (conditional-alternate body) (list gensym)))
               (not (lexical-assigned? gensym))
               (cons a (conditional-alternate body))))
         (_ #f))))

;; Can X be compiled into an expression without needing temporary
;; variables? This is only used to produce nicer code.
(define (expression-like? x)
  (cond
   ((or (const? x) (void? x) (lexical-ref? x) (toplevel-ref? x)
        (module-ref? x) (primitive-ref? x) (lambda? x))
    #t)
   ((call? x)
    (and (not (throw-call? x))
         (not (try-call? x))
         (expression-like? (call-proc x))
         (every expression-like? (call-args x))))
   ((primcall? x) (every expression-like? (primcall-args x)))
   ((conditional? x)
    (and (expression-like? (conditional-test x))
         (expression-like? (conditional-consequent x))
         (expression-like? (conditional-alternate x))))
   ((or-pattern x)
    => (match-lambda ((a . b) (and (expression-like? a) (expression-like? b)))))
   ((lexical-set? x) (expression-like? (lexical-set-exp x)))
   ((toplevel-set? x) (expression-like? (toplevel-set-exp x)))
   (else #f)))

;;
;; Statements
;;

(define (unsupported x)
  (compile-error
   (cond
    ((or (prompt? x) (abort? x))
     "continuations, escape procedures, exception handlers and dynamic-wind are not supported")
    ((let-values? x) "multiple values are not supported")
    ((module-set? x) "assigning variables of other modules is not supported")
    ((toplevel-define? x) "definitions are only allowed at the top level")
    (else (format #f "unsupported expression ~s" (unparse-tree-il x))))))

(define (compile-stmt x ctx acc)
  (note-location! (tree-il-src x))
  (call-with-statement-location x (lambda () (compile-stmt* x ctx acc))))

(define (compile-stmt* x ctx acc)
  (cond
   ((void? x) (deliver! ctx #f acc))
   ((seq? x)
    (compile-stmt (seq-head x) 'effect acc)
    (compile-stmt (seq-tail x) ctx acc))
   ((conditional? x) (compile-conditional x ctx acc))
   ((and (or-pattern x) (null? (ctx-loops ctx)) (expression-like? x))
    (deliver! ctx (compile-value x acc) acc))
   ((let? x)
    (compile-let-bindings! x acc)
    (compile-stmt (let-body x) ctx acc))
   ((letrec? x)
    (if (inline-loop? x)
        (compile-inline-loop x ctx acc)
        (begin
          (compile-letrec-bindings! x acc)
          (compile-stmt (letrec-body x) ctx acc))))
   ((call? x) (compile-call-stmt x ctx acc))
   ((lexical-set? x)
    (compile-assignment! (gensym-name (lexical-set-gensym x)) (lexical-set-exp x) acc)
    (deliver! ctx #f acc))
   ((toplevel-set? x)
    (compile-assignment! (toplevel-js-name (toplevel-set-name x)) (toplevel-set-exp x) acc)
    (deliver! ctx #f acc))
   ((or (prompt? x) (abort? x) (let-values? x) (module-set? x) (toplevel-define? x) (fix? x))
    (unsupported x))
   (else (deliver! ctx (compile-value x acc) acc))))

(define (constant-truth x)
  (and (const? x)
       (match (const-exp x)
         (#t 'true)
         (#f 'false)
         (_ #f))))

(define (exits? stmt)
  (match stmt
    ((or ('return _) ('throw _) ('break _) ('continue _)) #t)
    (('try (? pair? body) _ (? pair? handler))
     (and (exits? (last body)) (exits? (last handler))))
    (('if _ (? pair? then) (? pair? else))
     (and (exits? (last then)) (exits? (last else))))
    (_ #f)))

(define (assign-stmts name e)
  (if (equal? e '(undefined))
      '()
      `((expr-stmt (assign (id ,name) ,e)))))

(define (emit-if! test then else acc)
  (match (list then else)
    ((() ())
     (unless (pure-expr? test)
       (emit! acc `(expr-stmt ,test))))
    (((('return (? identity a))) (('return (? identity b))))
     (emit! acc `(return (ternary ,test ,a ,b))))
    (((('expr-stmt ('assign ('id n) a))) (('expr-stmt ('assign ('id m) b))))
     (=> next)
     (if (string=? n m)
         (emit! acc `(expr-stmt (assign (id ,n) (ternary ,test ,a ,b))))
         (next)))
    ((() _) (emit! acc `(if ,(js-negate test) ,else #f)))
    ((_ ()) (emit! acc `(if ,test ,then #f)))
    (_
     (if (exits? (last then))
         (begin
           ;; No need for an else branch if the first branch never falls
           ;; through.
           (emit! acc `(if ,test ,then #f))
           (emit-all! acc else))
         (emit! acc `(if ,test ,then ,else))))))

(define (compile-conditional x ctx acc)
  (match (constant-truth (conditional-test x))
    ('true (compile-stmt (conditional-consequent x) ctx acc))
    ('false (compile-stmt (conditional-alternate x) ctx acc))
    (_
     (let ((test (compile-test (conditional-test x) acc))
           (then (new-acc))
           (else (new-acc)))
       (compile-stmt (conditional-consequent x) ctx then)
       (compile-stmt (conditional-alternate x) ctx else)
       (emit-if! test (acc-stmts then) (acc-stmts else) acc)))))

(define (compile-call-stmt x ctx acc)
  (let ((proc (call-proc x))
        (args (call-args x)))
    (cond
     ((find-loop ctx proc)
      => (lambda (loop) (compile-loop-continue loop args ctx acc)))
     ((try-call? x) (compile-try args ctx acc))
     ((and (call-with-values? x) (values-clause (cadr args)))
      (compile-stmt (compile-values-bindings! args acc) ctx acc))
     ((throw-call? x) (compile-throw x acc))
     (else (deliver! ctx (compile-value x acc) acc)))))

;;
;; Multiple values
;;
;; Multiple values are arrays (a single value is just the value), so
;; (call-with-values producer (lambda (a b . rest) body)) =>
;;
;;   const [a, b, ...rest] = <producer value>;
;;   body

;; The value of PRODUCER (already compiled into PRODUCER-JS).
(define (producer-value producer producer-js)
  (if (thunk-body producer)
      (match producer-js
        (('arrow () (('return e))) e)
        (_ `(call ,producer-js)))
      `(call ,producer-js)))

;; Emit the bindings of the consumer (a lambda literal) and return its body.
(define (compile-values-bindings! args acc)
  (match args
    ((producer consumer)
     (let* ((clause (values-clause consumer))
            (req (lambda-case-req clause))
            (rest (lambda-case-rest clause))
            (gensyms (lambda-case-gensyms clause))
            (body (thunk-body producer)))
       (cond
        ;; A single value.
        ((and (= 1 (length req)) (not rest))
         (let ((gensym (car gensyms)))
           (if body
               (compile-declaration! (binding-kind gensym) (bind-local! gensym (car req)) body acc)
               (emit! acc `(decl ,(binding-kind gensym) ,(bind-local! gensym (car req))
                                 (call ,(compile-value producer acc)))))))
        ;; No values.
        ((and (null? req) (not rest))
         (if body
             (compile-stmt body 'effect acc)
             (emit! acc `(expr-stmt (call ,(compile-value producer acc))))))
        (else
         (let* ((value (if body
                           (compile-value body acc)
                           `(call ,(compile-value producer acc))))
                (kind (if (any lexical-assigned? gensyms) "let" "const"))
                (names (map-in-order bind-local! (take gensyms (length req)) req))
                (rest-name (and rest (bind-local! (list-ref gensyms (length req)) rest))))
           (emit! acc `(decl-array ,kind ,names ,rest-name ,value)))))
       (lambda-case-body clause)))))

;; (%js-try (lambda () body) (lambda (e) handler)) =>
;;
;;   try { body } catch (e) { handler }
;;
;; Both body and handler deliver their value to CTX.
(define (compile-try args ctx acc)
  (match args
    (((? lambda? thunk) (? lambda? handler))
     (let ((body (lambda-body thunk))
           (catch (lambda-body handler)))
       (unless (and (simple-clause? body) (null? (lambda-case-req body))
                    (simple-clause? catch) (= 1 (length (lambda-case-req catch))))
         (compile-error "invalid guard"))
       (let ((body-acc (new-acc))
             (catch-acc (new-acc)))
         (compile-stmt (lambda-case-body body) ctx body-acc)
         (let ((name (bind! (car (lambda-case-gensyms catch)) (car (lambda-case-req catch)))))
           (compile-stmt (lambda-case-body catch) ctx catch-acc)
           (emit! acc `(try ,(acc-stmts body-acc) ,name ,(acc-stmts catch-acc)))))))
    (_ (compile-error "invalid guard"))))

(define (compile-throw x acc)
  (let ((args (call-args x)))
    (if (or (named-ref? (call-proc x) 'error) (named-ref? (call-proc x) 'throw))
        (let ((exprs (compile-exprs args acc)))
          (emit! acc `(throw (new (id "Error")
                                  ,(match exprs
                                     (() '(str "error"))
                                     ((msg) msg)
                                     (_ `(call (dot (array ,@exprs) "join")
                                               (str " "))))))))
        (match (compile-exprs args acc)
          ((obj) (emit! acc `(throw ,obj)))
          (_ (compile-error "raise expects one argument"))))))

;;
;; Loops
;;

(define (compile-inline-loop x ctx acc)
  (let* ((gensym (car (letrec-gensyms x)))
         (clause (lambda-body (car (letrec-vals x))))
         (req (lambda-case-req clause))
         (gensyms (lambda-case-gensyms clause))
         (body (lambda-case-body clause))
         (inits (call-args (letrec-body x)))
         (target `(lexical . ,gensym)))
    (if (zero? (count-tail-calls body target (length req)))
        ;; The function never calls itself, so it's just a let.
        (begin
          (for-each (lambda (gensym name init)
                      (compile-declaration! (binding-kind gensym) (bind! gensym name) init acc))
                    gensyms req inits)
          (compile-stmt body ctx acc))
        (let* ((init-exprs (compile-exprs inits acc))
               (vars (map-in-order declare-name! req))
               (label (cons (declare-name! (car (letrec-names x))) #f))
               (loop (make-loop target vars label))
               (body-acc (new-acc)))
          (for-each (lambda (var e) (emit! acc `(decl "let" ,var ,e))) vars init-exprs)
          (bind-loop-variables! gensyms req vars body body-acc)
          (compile-stmt body (cons loop ctx) body-acc)
          (emit! acc `(while ,label ,(loop-body (acc-stmts body-acc))))))))

;; Loop variables are reassigned on each iteration. If they are captured by
;; a closure we need a fresh binding per iteration (like Scheme does).
(define (bind-loop-variables! gensyms names vars body acc)
  (if (captures-any? body gensyms)
      (for-each (lambda (gensym name var)
                  (emit! acc `(decl ,(binding-kind gensym) ,(bind! gensym name) (id ,var))))
                gensyms names vars)
      (for-each set-gensym-name! gensyms vars)))

(define (js-mentions? e name)
  (let walk ((e e))
    (cond
     ((and (pair? e) (eq? (car e) 'id) (pair? (cdr e)) (equal? (cadr e) name)) #t)
     ((pair? e) (or (walk (car e)) (walk (cdr e))))
     (else #f))))

(define (compile-loop-continue loop args ctx acc)
  (let* ((exprs (compile-exprs args acc))
         (changes (filter-map (lambda (var e)
                                (and (not (equal? e `(id ,var))) (cons var e)))
                              (loop-vars loop) exprs)))
    (emit-parallel-assignments! changes acc)
    (emit! acc (loop-jump 'continue loop ctx))))

;; Assign all the (VAR . VALUE) pairs as if it was done in parallel. A
;; variable can be assigned once no other pending value refers to it. If
;; there's a cycle, the old value of a variable is saved in a temporary.
(define (emit-parallel-assignments! changes acc)
  (let lp ((pending changes))
    (unless (null? pending)
      (let ((ready (find (lambda (change)
                           (not (any (lambda (other)
                                       (and (not (eq? other change))
                                            (js-mentions? (cdr other) (car change))))
                                     pending)))
                         pending)))
        (if ready
            (begin
              (emit! acc `(expr-stmt (assign (id ,(car ready)) ,(cdr ready))))
              (lp (delete ready pending eq?)))
            (let* ((var (car (car pending)))
                   (old (hoist! `(id ,var) acc)))
              (lp (map (lambda (change)
                         (if (eq? change (car pending))
                             change
                             (cons (car change)
                                   (substitute (cdr change) `((,var . ,old))))))
                       pending))))))))

;; A while loop body, without a redundant continue at the end.
(define (loop-body stmts)
  (if (and (pair? stmts) (equal? (last stmts) '(continue #f)))
      (drop-right stmts 1)
      stmts))

;;
;; Functions
;;

(define (compile-function-declaration name x target)
  (let-values (((params body) (compile-lambda x target)))
    (if (async-lambda? x)
        `(async-fun-decl ,name ,params ,body)
        `(fun-decl ,name ,params ,body))))

(define (compile-lambda-expression x)
  (let-values (((params body) (compile-lambda x #f)))
    (if (async-lambda? x)
        `(async (arrow ,params ,body))
        `(arrow ,params ,body))))

;; Returns the parameters and the body statements of a function.
(define (compile-lambda x target)
  (call-with-function-scope
   (lambda ()
     (let ((clause (lambda-body x)))
       (cond
        ((not (lambda-case? clause))
         (values '() '((throw (new (id "Error") (str "wrong number of arguments"))))))
        ((lambda-case-alternate clause) (compile-case-lambda clause))
        (else (compile-clause clause target)))))))

(define (compile-default init)
  (if (and (const? init) (eq? (const-exp init) #f))
      #f
      (let* ((acc (new-acc))
             (e (compile-value init acc)))
        (if (acc-empty? acc)
            e
            `(call (arrow () ,(append (acc-stmts acc) `((return ,e)))))))))

(define (compile-clause clause target)
  (let* ((req (lambda-case-req clause))
         (opt (or (lambda-case-opt clause) '()))
         (rest (lambda-case-rest clause))
         (kw (match (lambda-case-kw clause) (#f '()) ((_ . entries) entries)))
         (gensyms (lambda-case-gensyms clause))
         (nreq (length req))
         (nopt (length opt))
         (req-gensyms (take gensyms nreq))
         (opt-gensyms (take (drop gensyms nreq) nopt))
         (rest-gensym (and rest (list-ref gensyms (+ nreq nopt))))
         (inits (lambda-case-inits clause))
         (body (lambda-case-body clause))
         (acc (new-acc)))
    (when (and (pair? opt) (pair? kw))
      (compile-error "#:optional and #:key arguments can't be combined"))
    (when (and rest (pair? kw))
      (compile-error "rest and #:key arguments can't be combined"))
    (if (and target
             (simple-clause? clause)
             (positive? (or (count-tail-calls body target nreq) 0)))
        ;; A function that calls itself in tail position: a loop.
        (let* ((vars (map-in-order declare-name! req))
               (label (cons (declare-name! (match target
                                             (('toplevel . name) name)
                                             (('lexical . _) "loop")))
                            #f))
               (loop (make-loop target vars label))
               (body-acc (new-acc)))
          (bind-loop-variables! req-gensyms req vars body body-acc)
          (compile-stmt body (cons loop 'return) body-acc)
          (values (map (lambda (var) `(param ,var)) vars)
                  `((while ,label ,(loop-body (acc-stmts body-acc))))))
        (let* ((req-params (map-in-order (lambda (gensym name) `(param ,(bind! gensym name)))
                                         req-gensyms req))
               (opt-params (map-in-order (lambda (gensym name init)
                                  (let ((default (compile-default init)))
                                    (if default
                                        `(param ,(bind! gensym name) ,default)
                                        `(param ,(bind! gensym name)))))
                                opt-gensyms opt (take inits nopt)))
               (rest-params (if rest `((rest ,(bind! rest-gensym rest))) '()))
               (kw-params (if (null? kw)
                              '()
                              `((options
                                 ,(map-in-order (match-lambda*
                                         (((keyword name gensym) init)
                                          (let ((default (compile-default init)))
                                            (list (keyword->js-name keyword)
                                                  (bind! gensym name)
                                                  default))))
                                       kw (drop inits nopt)))))))
          (compile-stmt body 'return acc)
          (values (append req-params opt-params rest-params kw-params)
                  (acc-stmts acc))))))

;; case-lambda: dispatch on the number of arguments.
(define (compile-case-lambda clause)
  (let ((args (fresh-name "args")))
    (define (compile-branch clause)
      (when (or (lambda-case-opt clause) (lambda-case-kw clause))
        (compile-error "case-lambda clauses with #:optional or #:key arguments are not supported"))
      (let* ((req (lambda-case-req clause))
             (n (length req))
             (rest (lambda-case-rest clause))
             (gensyms (lambda-case-gensyms clause))
             (acc (new-acc)))
        (for-each (lambda (gensym name i)
                    (emit! acc `(decl ,(binding-kind gensym) ,(bind! gensym name)
                                      (index (id ,args) (num ,i)))))
                  (take gensyms n) req (iota n))
        (when rest
          (let ((gensym (list-ref gensyms n)))
            (emit! acc `(decl ,(binding-kind gensym) ,(bind! gensym rest)
                              (call (dot (id ,args) "slice") (num ,n))))))
        (compile-stmt (lambda-case-body clause) 'return acc)
        (cons `(binop ,(if rest ">=" "===") (dot (id ,args) "length") (num ,n))
              (acc-stmts acc))))
    (values
     `((rest ,args))
     (list
      (let lp ((clause clause))
        (if clause
            (match (compile-branch clause)
              ((test . stmts)
               `(if ,test ,stmts ,(list (lp (lambda-case-alternate clause))))))
            '(throw (new (id "Error") (str "wrong number of arguments")))))))))

;;
;; Values
;;

;; JavaScript has no pairs: quoted data is always made of arrays (lists and
;; vectors), so dotted pairs are a compilation error.
(define (dotted-pair? v)
  (and (pair? v) (not (list? v))))

(define (dotted-pair-error v)
  (compile-error "the pair ~s has no JavaScript equivalent (JavaScript has no pairs): use a list, e.g. ~s"
                 v (let lp ((v v))
                     (if (pair? v) (cons (car v) (lp (cdr v))) (list v)))))

;; e.g. '(("all" . "All") ...)
(define (association-list-error alist)
  (match (find dotted-pair? alist)
    ((key . value)
     (compile-error "quoted association lists have no JavaScript equivalent (JavaScript has no pairs): use a list of lists to iterate it, e.g. '(~s ...), or js-obj for an object, e.g. (js-obj ~a ~s ...)"
                    (list key value)
                    (if (symbol? key) (format #f "'~s" key) (format #f "~s" key))
                    value))))

(define (compile-constant v)
  (cond
   ((eq? v #t) '(bool #t))
   ((eq? v #f) '(bool #f))
   ((eq? v #nil) '(null))
   ((null? v) '(array))
   ((unspecified? v) '(undefined))
   ((and (number? v) (real? v)) `(num ,v))
   ((string? v) `(str ,v))
   ((char? v) `(str ,(string v)))
   ((symbol? v) `(str ,(symbol->string v)))
   ((keyword? v) `(str ,(symbol->string (keyword->symbol v))))
   ((vector? v) `(array ,@(map compile-constant (vector->list v))))
   ((and (pair? v) (list? v) (every pair? v) (any dotted-pair? v))
    (association-list-error v))
   ((list? v) `(array ,@(map compile-constant v)))
   ((pair? v) (dotted-pair-error v))
   (else (compile-error "unsupported constant ~s" v))))

(define (primitive-value prim)
  (cond
   ((primitive-value-builder prim)
    => (lambda (build) (call-with-function-scope (lambda () (build fresh-name)))))
   (else
    (call-with-function-scope
     (lambda ()
       (let* ((params (map-in-order (lambda (_) (fresh-name "x"))
                                    (iota (primitive-min-args prim))))
              (args (map (lambda (p) `(id ,p)) params)))
         `(arrow ,(map (lambda (p) `(param ,p)) params)
                 ((return ,((primitive-compiler prim) args (map (const #f) params)
                            fresh-name))))))))))

(define (toplevel-value x)
  (let ((name (toplevel-ref-name x)))
    (cond
     ((primitive-for x) => primitive-value)
     ((property-ref x)
      ;; .-prop as a value: (obj) => obj.prop
      => (lambda (prop)
           (call-with-function-scope
            (lambda ()
              (let ((obj (fresh-name "obj")))
                `(arrow ((param ,obj)) ((return (dot (id ,obj) ,prop)))))))))
     ((method-ref x)
      ;; .method as a value: (obj, ...args) => obj.method(...args)
      => (lambda (method)
           (call-with-function-scope
            (lambda ()
              (let ((obj (fresh-name "obj"))
                    (args (fresh-name "args")))
                `(arrow ((param ,obj) (rest ,args))
                        ((return (call (dot (id ,obj) ,method) (spread (id ,args)))))))))))
     ((and (memq name %special-procedures) (not (user-defined? name)))
      (compile-error "`~a' can only be called, it can't be used as a value" name))
     (else `(id ,(toplevel-js-name name))))))

(define (compile-value x acc)
  (note-location! (tree-il-src x))
  (call-with-statement-location x (lambda () (compile-value* x acc))))

(define (compile-value* x acc)
  (cond
   ((const? x) (compile-constant (const-exp x)))
   ((void? x) '(undefined))
   ((lexical-ref? x) `(id ,(gensym-name (lexical-ref-gensym x))))
   ((toplevel-ref? x) (toplevel-value x))
   ((or (module-ref? x) (primitive-ref? x))
    (let ((prim (primitive-for x)))
      (if prim
          (primitive-value prim)
          (compile-error "`~a' is not supported by GuileScript"
                         (if (module-ref? x) (module-ref-name x) (primitive-ref-name x))))))
   ((lambda? x) (compile-lambda-expression x))
   ((call? x) (compile-call-value x acc))
   ((primcall? x)
    (let ((prim (lookup-primitive (primcall-name x))))
      (unless prim
        (compile-error "`~a' is not supported by GuileScript" (primcall-name x)))
      (compile-primitive-call prim (primcall-args x) acc)))
   ((lexical-set? x)
    `(assign (id ,(gensym-name (lexical-set-gensym x)))
             ,(compile-value (lexical-set-exp x) acc)))
   ((toplevel-set? x)
    `(assign (id ,(toplevel-js-name (toplevel-set-name x)))
             ,(compile-value (toplevel-set-exp x) acc)))
   ((seq? x)
    (compile-stmt (seq-head x) 'effect acc)
    (compile-value (seq-tail x) acc))
   ((conditional? x) (compile-conditional-value x acc))
   ((or-pattern x) => (lambda (pattern) (compile-or pattern acc)))
   ((let? x)
    (compile-let-bindings! x acc)
    (compile-value (let-body x) acc))
   ((and (letrec? x) (not (inline-loop? x)))
    (compile-letrec-bindings! x acc)
    (compile-value (letrec-body x) acc))
   ((or (prompt? x) (abort? x) (let-values? x) (module-set? x) (toplevel-define? x) (fix? x))
    (unsupported x))
   (else
    ;; Anything else needs statements (e.g. loops): use a temporary.
    (let ((t (fresh-name "t")))
      (emit! acc `(decl "let" ,t #f))
      (compile-stmt x `(assign . ,t) acc)
      `(id ,t)))))

;; Compile expressions left to right. If an expression needs statements, the
;; previous (non-simple) expressions are evaluated first into temporaries to
;; keep the evaluation order.
(define (compile-exprs xs acc)
  (let lp ((xs xs) (done '()))
    (if (null? xs)
        (reverse done)
        (let* ((sub (new-acc))
               (e (compile-value (car xs) sub)))
          (if (acc-empty? sub)
              (lp (cdr xs) (cons e done))
              (let ((done (reverse
                           (map (lambda (d) (if (simple-expr? d) d (hoist! d acc)))
                                (reverse done)))))
                (emit-all! acc (acc-stmts sub))
                (lp (cdr xs) (cons e done))))))))

;; The value of a test, where only its truthiness matters.
(define (compile-test x acc)
  (cond
   ;; (and a b) => (if a b #f)
   ((and (conditional? x) (eq? (constant-truth (conditional-alternate x)) 'false))
    (let* ((a (compile-test (conditional-test x) acc))
           (sub (new-acc))
           (b (compile-test (conditional-consequent x) sub)))
      (if (acc-empty? sub)
          `(binop "&&" ,a ,b)
          (compile-value x acc))))
   (else (compile-value x acc))))

(define (compile-or pattern acc)
  (match pattern
    ((a-x . b-x)
     (let* ((a (compile-test a-x acc))
            (sub (new-acc))
            (b (compile-value b-x sub)))
       (if (acc-empty? sub)
           `(binop "||" ,a ,b)
           (let ((a (if (simple-expr? a) a (hoist! a acc)))
                 (t (fresh-name "t")))
             (emit! acc `(decl "let" ,t #f))
             (emit-if! a (assign-stmts t a) (append (acc-stmts sub) (assign-stmts t b)) acc)
             `(id ,t)))))))

(define (compile-conditional-value x acc)
  (match (constant-truth (conditional-test x))
    ('true (compile-value (conditional-consequent x) acc))
    ('false (compile-value (conditional-alternate x) acc))
    (_
     (let* ((test (compile-test (conditional-test x) acc))
            (then-acc (new-acc))
            (else-acc (new-acc))
            (then (compile-value (conditional-consequent x) then-acc))
            (else (compile-value (conditional-alternate x) else-acc)))
       (if (and (acc-empty? then-acc) (acc-empty? else-acc))
           `(ternary ,test ,then ,else)
           (let ((t (fresh-name "t")))
             (emit! acc `(decl "let" ,t #f))
             (emit-if! test
                       (append (acc-stmts then-acc) (assign-stmts t then))
                       (append (acc-stmts else-acc) (assign-stmts t else))
                       acc)
             `(id ,t)))))))

;;
;; Calls
;;

;; Trailing keyword arguments (f a #:key value ...) become an options object:
;; f(a, { key: value }). Returns the positional arguments and a list of
;; (KEY . VALUE) pairs.
(define (split-keyword-args args)
  (define (keyword-pairs args)
    (match args
      (() '())
      (((? keyword-const? kw) value . rest)
       (let ((pairs (keyword-pairs rest)))
         (and pairs (cons (cons (keyword->js-name (const-exp kw)) value) pairs))))
      (_ #f)))
  (let lp ((args args) (positional '()))
    (match args
      (() (values (reverse positional) '()))
      (((? keyword-const?) . _)
       (let ((pairs (keyword-pairs args)))
         (if pairs
             (values (reverse positional) pairs)
             (lp (cdr args) (cons (car args) positional)))))
      ((arg . rest) (lp rest (cons arg positional))))))

(define (spread-args exprs)
  (append (drop-right exprs 1)
          (match (last exprs)
            (('array . elts) elts)
            (e `((spread ,e))))))

(define (compile-call-value x acc)
  (let ((proc (call-proc x))
        (args (call-args x)))
    (cond
     ((throw-call? x)
      (compile-throw x acc)
      '(undefined))
     ((try-call? x)
      (let ((t (fresh-name "t")))
        (emit! acc `(decl "let" ,t #f))
        (compile-try args `(assign . ,t) acc)
        `(id ,t)))
     ((call-with-values? x)
      (if (values-clause (cadr args))
          (compile-value (compile-values-bindings! args acc) acc)
          ;; The values (an array) are the arguments of the consumer.
          (match (compile-exprs args acc)
            ((producer consumer)
             (call-function consumer
                            (match (producer-value (car args) producer)
                              (('array . elts) elts)
                              (e `((spread ,e)))))))))
     ((primitive-for proc)
      => (lambda (prim) (compile-primitive-call prim args acc)))
     ((named-ref? proc 'apply) (compile-apply args acc))
     ((record-procedure-for proc)
      => (lambda (def) (compile-record-call def args acc)))
     ((method-ref proc)
      => (lambda (method) (compile-method-call method args acc)))
     ((property-ref proc)
      => (lambda (prop)
           (match (compile-exprs args acc)
             ((obj) `(dot ,obj ,prop))
             (_ (compile-error "(.-~a obj) needs exactly one object" prop)))))
     ((setter-target proc)
      => (lambda (target) (compile-setter target args acc)))
     ((named-ref? proc 'js-import)
      (compile-error "js-import is only allowed at the top level"))
     ((named-ref? proc 'process-use-modules)
      (compile-error "use-modules is only allowed at the top level"))
     ((named-ref? proc 'js-global)
      (compile-error "js-global is only allowed at the top level"))
     ((named-ref? proc '%js-values-ref)
      (match args
        ((values (? const? vars) (? const? var))
         `(index ,(compile-value values acc)
                 (num ,(list-index (cut eq? <> (const-exp var)) (const-exp vars)))))))
     ((named-ref? proc '%js-values-rest)
      (match args
        ((values (? const? vars))
         `(call (dot ,(compile-value values acc) "slice") (num ,(length (const-exp vars)))))))
     ((named-ref? proc 'js-await)
      (match args
        ((promise) `(unop "await " ,(compile-value promise acc)))
        (_ (compile-error "js-await expects one argument"))))
     ((and (lexical-ref? proc)
           (not (hashq-ref (unit-gensym-names (current-unit)) (lexical-ref-gensym proc) #f)))
      (compile-error "internal error: call to an inlined loop outside of a tail position"))
     (else
      (let-values (((positional keywords) (split-keyword-args args)))
        (match (compile-exprs (append (list proc) positional (map cdr keywords)) acc)
          ((f . exprs)
           (let ((n (length positional)))
             (call-function f
                            (append
                             (take exprs n)
                             (if (null? keywords)
                                 '()
                                 `((object ,@(map (lambda (kw e) (cons (car kw) e))
                                                  keywords (drop exprs n)))))))))))))))

;; (set! (.-prop obj) value) => obj.prop = value
;; (set! (js-ref obj 'a 'b) value) => obj.a.b = value
(define (compile-setter target args acc)
  (cond
   ((property-ref target)
    => (lambda (prop)
         (match (compile-exprs args acc)
           ((obj value) `(assign (dot ,obj ,prop) ,value))
           (_ (compile-error "(set! (.-~a obj) value) needs an object and a value" prop)))))
   ((and (named-ref? target 'js-ref) (primitive-for target))
    (compile-primitive-call (lookup-primitive 'js-set!) args acc))
   (else
    (compile-error "set! is only supported on variables, (.-prop obj) and (js-ref obj ...)"))))

;; (.method obj arg ...) => obj.method(arg, ...)
(define (compile-method-call method args acc)
  (when (null? args)
    (compile-error "(.~a obj ...) needs an object to call the method on" method))
  (let-values (((positional keywords) (split-keyword-args (cdr args))))
    (match (compile-exprs (append (list (car args)) positional (map cdr keywords)) acc)
      ((obj . exprs)
       (let ((n (length positional)))
         `(call (dot ,obj ,method)
                ,@(take exprs n)
                ,@(if (null? keywords)
                      '()
                      `((object ,@(map (lambda (kw e) (cons (car kw) e))
                                       keywords (drop exprs n)))))))))))

(define (compile-apply args acc)
  (when (< (length args) 2)
    (compile-error "apply needs a procedure and a list of arguments"))
  (let ((prim (primitive-for (car args))))
    (cond
     (prim
      (call-function (primitive-value prim) (spread-args (compile-exprs (cdr args) acc))))
     ;; (apply .method obj arg ... list) => obj.method(arg, ..., ...list)
     ((method-ref (car args))
      => (lambda (method)
           (when (< (length args) 3)
             (compile-error "(apply .~a obj ...) needs an object" method))
           (match (spread-args (compile-exprs (cdr args) acc))
             ((('spread obj) . _)
              (compile-error "(apply .~a obj ...) needs an object" method))
             ((obj . rest) `(call (dot ,obj ,method) ,@rest)))))
     (else
      (match (compile-exprs args acc)
        ((f . exprs) (call-function f (spread-args exprs))))))))

(define (compile-primitive-call prim args acc)
  (let ((argc (length args))
        (min (primitive-min-args prim))
        (max (primitive-max-args prim)))
    (when (or (< argc min) (and max (> argc max)))
      (compile-error "wrong number of arguments to `~a'" (primitive-name prim)))
    (let* ((dups (primitive-dup-indices prim argc))
           (exprs (compile-exprs args acc))
           ;; Arguments used more than once are evaluated into temporaries
           ;; (together with the previous ones, to keep the evaluation
           ;; order).
           (last-dup (fold (lambda (i e last)
                             (if (and (memv i dups) (not (simple-expr? e))) i last))
                           -1 (iota argc) exprs))
           (exprs (map (lambda (i e)
                         (if (and (<= i last-dup) (not (simple-expr? e)))
                             (hoist! e acc)
                             e))
                       (iota argc) exprs)))
      ;; Names created by primitives are parameters of the functions they
      ;; create, so they don't need to be reserved in the current scope.
      (call-with-function-scope
       (lambda () ((primitive-compiler prim) exprs args fresh-name))))))
