;;; (language javascript primitives) --- GuileScript.

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

;; Primitives are Scheme procedures that are compiled inline into JavaScript
;; expressions, so no runtime library is needed.
;;
;; GuileScript uses JavaScript data and semantics: lists and vectors are
;; arrays, symbols and characters are strings and hash tables are Maps.
;; JavaScript has no pairs, so there are no association lists: objects are
;; created with js-obj. Like in Scheme, only #f, #nil (null) and undefined
;; are false (see truth).
;;
;; Each primitive has a compiler procedure that receives:
;;
;;   ARGS   the arguments already compiled to JavaScript expressions
;;   TIL    the original Tree-IL arguments (an element can be #f when the
;;          primitive is used as a first-class value)
;;   FRESH  a procedure that returns a fresh JavaScript variable name
;;
;; Arguments listed in DUP are used more than once in the result, so the
;; compiler makes sure they are simple (variables, literals, functions or
;; property accesses) before calling the primitive compiler.
;;
;; A few primitives need a helper function (e.g. equal? needs isEqual to
;; compare arrays). Helpers are added to the modules that use them.

;;; Code:

(define-module (language javascript primitives)
  #:use-module (ice-9 match)
  #:use-module (language tree-il)
  #:use-module (language javascript errors)
  #:use-module (language javascript names)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export (lookup-primitive
            primitive-name
            primitive-min-args
            primitive-max-args
            primitive-dup-indices
            primitive-compiler
            primitive-value-builder
            primitive-helpers
            current-helper-ref
            helper-js-name
            helper-definition
            call-function
            js-negate
            boolean-expr?
            current-boolean-name?
            truth
            substitute))

(define-record-type <primitive>
  (make-primitive name min max dup compiler value helpers)
  primitive?
  (name primitive-name)
  (min primitive-min-args)
  (max primitive-max-args)              ; #f means variadic
  (dup primitive-dup)                   ; #f, 'all, list or procedure
  (compiler primitive-compiler)
  (value primitive-value-builder)       ; #f or (lambda (fresh) ...)
  (helpers primitive-helpers))          ; helpers the primitive may use

(define primitives (make-hash-table))

(define (lookup-primitive name)
  (hashq-ref primitives name #f))

;; The indices of the arguments that need to be simple for a call with
;; ARGC arguments.
(define (primitive-dup-indices prim argc)
  (let ((dup (primitive-dup prim)))
    (match (if (procedure? dup) (dup argc) dup)
      (#f '())
      ('all (iota argc))
      ((? list? indices) indices))))

(define* (define-primitive! names min max compiler #:key dup value (helpers '()))
  (for-each (lambda (name)
              (hashq-set! primitives name
                          (make-primitive name min max dup compiler value helpers)))
            (if (list? names) names (list names))))

;;
;; Helper functions added to the compiled modules (e.g. isEqual)
;;

;; A procedure that returns the JavaScript variable of a helper (and adds
;; the helper to the module).
(define current-helper-ref (make-parameter #f))

(define (helper-ref helper)
  ((current-helper-ref) helper))

(define (helper-js-name helper)
  (match helper
    ('equal "isEqual")))

;; The definition of HELPER, a function named NAME.
(define (helper-definition helper name)
  (match helper
    ;; function isEqual(a, b) {
    ;;   if (Array.isArray(a) && Array.isArray(b)) {
    ;;     return a.length === b.length && a.every((x, i) => isEqual(x, b[i]));
    ;;   }
    ;;   return a === b;
    ;; }
    ('equal
     `(fun-decl ,name ((param "a") (param "b"))
                ((if (binop "&&"
                            (call (dot (id "Array") "isArray") (id "a"))
                            (call (dot (id "Array") "isArray") (id "b")))
                     ((return (binop "&&"
                                     (binop "===" (dot (id "a") "length") (dot (id "b") "length"))
                                     (call (dot (id "a") "every")
                                           (arrow ((param "x") (param "i"))
                                                  ((return (call (id ,name) (id "x")
                                                                 (index (id "b") (id "i"))))))))))
                     #f)
                 (return (binop "===" (id "a") (id "b"))))))))

;;
;; Building and inspecting the JavaScript AST
;;

;; "Math.max" => (dot (id "Math") "max")
(define (js-path path)
  (let ((parts (string-split path #\.)))
    (fold (lambda (part e) `(dot ,e ,part))
          `(id ,(car parts))
          (cdr parts))))

(define (num n) `(num ,n))
(define (str s) `(str ,s))
(define (binop op a b) `(binop ,op ,a ,b))
(define (unop op a) `(unop ,op ,a))
(define (call f . args) `(call ,f ,@args))
(define (method obj name . args) `(call (dot ,obj ,name) ,@args))
(define (prop obj name) `(dot ,obj ,name))
(define (arrow params body) `(arrow ,(map (lambda (p) `(param ,p)) params)
                                    ((return ,body))))

;; The name of a constant symbol, string or keyword, or #f.
(define (constant-name til)
  (and til (const? til)
       (let ((v (const-exp til)))
         (cond
          ((symbol? v) (symbol->string v))
          ((string? v) v)
          ((keyword? v) (keyword->js-name v))
          (else #f)))))

;; obj.name, obj["some-name"] or obj[expr].
(define (member-access obj key-js key-til)
  (let ((name (constant-name key-til)))
    (cond
     ((and name (js-identifier-name? name)) `(dot ,obj ,name))
     (name `(index ,obj (str ,name)))
     (else `(index ,obj ,key-js)))))

(define (left-fold op args)
  (fold (lambda (arg acc) (binop op acc arg)) (car args) (cdr args)))

(define (array-elements e)
  (match e
    (('array . elts) elts)
    (_ `((spread ,e)))))

;; A function expression taking exactly N plain parameters.
(define (function-with-arity? e n)
  (match e
    (('async f) (function-with-arity? f n))
    (('arrow params _)
     (and (= n (length params))
          (every (match-lambda (('param _) #t) (_ #f)) params)))
    (_ #f)))

(define (tree-contains? pred e)
  (let walk ((e e))
    (or (pred e)
        (and (pair? e) (or (walk (car e)) (walk (cdr e)))))))

(define (substitute e mapping)
  (let walk ((e e))
    (match e
      (('id name) (or (assoc-ref mapping name) e))
      ((a . b) (cons (walk a) (walk b)))
      (_ e))))

;; How many times is (id NAME) used in E? With #:in-functions? #f, uses
;; inside nested functions are not counted.
(define* (count-uses e name #:key (in-functions? #t))
  (let walk ((e e))
    (match e
      (('id (? (cut equal? <> name))) 1)
      (((or 'arrow 'async) . _) (if in-functions? (walk (cdr e)) 0))
      ((a . b) (+ (walk a) (walk b)))
      (_ 0))))

(define (simple-argument? e)
  (match e
    ((or ('id _) ('num _) ('str _) ('bool _) ('null) ('undefined)) #t)
    (_ #f)))

;; Call F with ARGS. ((x) => x * 2)(y) is simplified to y * 2 when it's safe
;; to do so (simple arguments and a body without nested functions or
;; assignments).
(define (call-function f args)
  (match f
    ;; ((...xs) => xs.reduce(...))(...list) => list.reduce(...)
    (('arrow (('rest r)) (('return body)))
     (=> next)
     ;; The list is used once and not from a nested function (so no
     ;; variable can be captured).
     (if (and (= 1 (count-uses body r))
              (= 1 (count-uses body r #:in-functions? #f))
              (not (tree-contains? (match-lambda
                                     (((or 'assign 'ternary) . _) #t)
                                     (('binop (or "&&" "||") . _) #t)
                                     (_ #f))
                                   body)))
         (substitute body `((,r . ,(match args
                                    ((('spread l)) l)
                                    (_ `(array ,@args))))))
         (next)))
    (('arrow params (('return body)))
     (=> next)
     (if (and (function-with-arity? f (length args))
              (every simple-argument? args)
              (not (tree-contains? (match-lambda
                                     (((or 'arrow 'async 'assign) . _) #t)
                                     (_ #f))
                                   body)))
         (substitute body (map (lambda (p a) (cons (cadr p) a)) params args))
         (next)))
    (_ `(call ,f ,@args))))

;; If F-TIL is a primitive (e.g. + or odd?) used as a value, compile it
;; inline with the given ARGS instead of calling it.
(define (inline-primitive f f-til args fresh)
  (and f-til
       (not (match f (('id _) #t) (_ #f))) ; redefined by the user
       (let* ((name (cond
                     ((toplevel-ref? f-til) (toplevel-ref-name f-til))
                     ((module-ref? f-til) (module-ref-name f-til))
                     (else #f)))
              (prim (and name (lookup-primitive name)))
              (argc (length args)))
         (and prim
              (<= (primitive-min-args prim) argc)
              (or (not (primitive-max-args prim)) (<= argc (primitive-max-args prim)))
              ((primitive-compiler prim) args (map (const #f) args) fresh)))))

;; If F-TIL is a method (e.g. .toUpperCase) or a property (e.g. .-length)
;; used as a value, use it directly on the first argument.
(define (inline-method f f-til args)
  (and f-til
       (toplevel-ref? f-til)
       (match f (('arrow . _) #t) (_ #f)) ; not redefined by the user
       (pair? args)
       (let ((name (toplevel-ref-name f-til)))
         (cond
          ((method-symbol? name)
           `(call (dot ,(car args) ,(method-js-name name)) ,@(cdr args)))
          ((property-symbol? name)
           `(dot ,(car args) ,(property-js-name name)))
          (else #f)))))

(define (call-callback f f-til args fresh)
  (or (inline-primitive f f-til args fresh)
      (inline-method f f-til args)
      (call-function f args)))

;; JavaScript array methods pass extra arguments (index, array) to callbacks,
;; so a callback is wrapped unless it is known to take exactly N arguments.
(define (callback f f-til n fresh)
  (if (function-with-arity? f n)
      f
      (let* ((params (map-in-order (lambda (_) (fresh "x")) (iota n)))
             (args (map (lambda (p) `(id ,p)) params)))
        (arrow params (call-callback f f-til args fresh)))))

;; A two argument callback with its arguments swapped: (x acc) => (acc, x).
(define (swapped-callback f f-til fresh)
  (match f
    (('arrow (p1 p2) body)
     (=> next)
     (if (function-with-arity? f 2) `(arrow (,p2 ,p1) ,body) (next)))
    (('async ('arrow (p1 p2) body))
     (=> next)
     (if (function-with-arity? f 2) `(async (arrow (,p2 ,p1) ,body)) (next)))
    (_ (let ((a (fresh "acc")) (x (fresh "x")))
         (arrow (list a x) (call-callback f f-til (list `(id ,x) `(id ,a)) fresh))))))

(define (js-negate e)
  (match e
    (('bool b) `(bool ,(not b)))
    (('unop "!" x) x)
    (('binop "===" a b) `(binop "!==" ,a ,b))
    (('binop "!==" a b) `(binop "===" ,a ,b))
    (('binop "==" a b) `(binop "!=" ,a ,b))
    (('binop "!=" a b) `(binop "==" ,a ,b))
    ;; x != null && x !== false => x == null || x === false
    (('binop "&&" (and a ('binop (or "!=" "!==") . _)) (and b ('binop (or "!=" "!==") . _)))
     `(binop "||" ,(js-negate a) ,(js-negate b)))
    (_ `(unop "!" ,e))))

;;
;; Truthiness. Like in Scheme, only #f, #nil (null) and undefined are false:
;; 0, "" and NaN are true.
;;

;; A procedure that tells if a variable or function name is a boolean
;; (GuileScript predicates: its Scheme name ends in ?, e.g. even? => isEven).
(define current-boolean-name? (make-parameter (const #f)))

;; Methods that return booleans: JavaScript's isArray, has, includes... (and
;; GuileScript's, e.g. (.has-children? node) => node.hasChildren()).
(define (boolean-method-name? name)
  (define (prefixed? prefix)
    (let ((n (string-length prefix)))
      (and (string-prefix? prefix name)
           (> (string-length name) n)
           (char-upper-case? (string-ref name n)))))
  (or (member name '("has" "includes" "startsWith" "endsWith" "some" "every" "test"))
      (prefixed? "is")
      (prefixed? "has")))

;; Is the JavaScript expression E always a boolean?
(define (boolean-expr? e)
  (match e
    (('bool _) #t)
    (('unop "!" _) #t)
    (('binop (or "===" "!==" "==" "!=" "<" "<=" ">" ">=" "instanceof" "in") _ _) #t)
    (('binop (or "&&" "||") a b) (and (boolean-expr? a) (boolean-expr? b)))
    (('ternary _ a b) (and (boolean-expr? a) (boolean-expr? b)))
    ((or ('id name) ('call ('id name) . _)) ((current-boolean-name?) name))
    (('call ('dot _ name) . _) (boolean-method-name? name))
    (_ #f)))

;; Variables and property accesses (a.b.c) can be read twice.
(define (variable-access? e)
  (match e
    (('id _) #t)
    (('dot obj _) (variable-access? obj))
    (_ #f)))

;; A JavaScript expression that is true when E is true in Scheme (it is not
;; #f, #nil or undefined): E itself if it's a boolean, x != null && x !==
;; false for variables or (e ?? false) !== false.
(define (truth e)
  (match e
    ((? boolean-expr?) e)
    ((or ('num _) ('str _) ('arrow . _) ('async . _)) '(bool #t))
    ((or ('null) ('undefined)) '(bool #f))
    ((? variable-access?)
     `(binop "&&" (binop "!=" ,e (null)) (binop "!==" ,e (bool #f))))
    (_ `(binop "!==" (binop "??" ,e (bool #f)) (bool #f)))))

;; Return statements in STMTS (not in nested functions) return the Scheme
;; truth of their value.
(define (truth-returns stmts)
  (let walk ((e stmts))
    (match e
      (('return #f) e)
      (('return v) `(return ,(truth v)))
      (((or 'arrow 'async 'fun-decl 'class) . _) e)
      ((a . b) (cons (walk a) (walk b)))
      (_ e))))

;; A callback that is used as a test (e.g. by filter): it returns a boolean.
(define (predicate-callback f f-til fresh)
  (match (callback f f-til 1 fresh)
    (('arrow params body) `(arrow ,params ,(truth-returns body)))
    (f f)))

;; A copy of the array E, unless E is already a new array.
(define (copy-array e)
  (match e
    (('array . _) e)
    (_ `(array (spread ,e)))))

(define (simple-compiler proc)
  (lambda (args til fresh) (apply proc args)))

(define-syntax-rule (define-inline names (arg ...) body ...)
  (define-primitive! 'names (length '(arg ...)) (length '(arg ...))
    (simple-compiler (lambda (arg ...) body ...))))

(define (define-js-function! names path)
  (define-primitive! names 0 #f
    (lambda (args til fresh) (apply call (js-path path) args))
    #:value (lambda (fresh) (js-path path))))

;;
;; Logging
;;

(define-js-function! 'console-log "console.log")
(define-js-function! 'console-debug "console.debug")
(define-js-function! 'console-info "console.info")
(define-js-function! 'console-warn "console.warn")
(define-js-function! 'console-error "console.error")

;;
;; Numbers
;;

;; (...xs) => xs.reduce((a, b) => a + b, 0)
(define (reduce-value op init)
  (lambda (fresh)
    (let ((xs (fresh "xs")) (a (fresh "a")) (b (fresh "b")))
      `(arrow ((rest ,xs))
              ((return ,(method `(id ,xs) "reduce"
                                (arrow (list a b) (binop op `(id ,a) `(id ,b)))
                                (num init))))))))

(define-primitive! '+ 0 #f
  (simple-compiler
   (lambda args (if (null? args) (num 0) (left-fold "+" args))))
  #:value (reduce-value "+" 0))

(define-primitive! '* 0 #f
  (simple-compiler
   (lambda args (if (null? args) (num 1) (left-fold "*" args))))
  #:value (reduce-value "*" 1))

(define (minus-value op one-arg)
  ;; (x, ...ys) => ys.length === 0 ? -x : ys.reduce((a, b) => a - b, x)
  (lambda (fresh)
    (let ((x (fresh "x")) (ys (fresh "ys")) (a (fresh "a")) (b (fresh "b")))
      `(arrow ((param ,x) (rest ,ys))
              ((return (ternary (binop "===" (dot (id ,ys) "length") (num 0))
                                ,(one-arg `(id ,x))
                                ,(method `(id ,ys) "reduce"
                                         (arrow (list a b) (binop op `(id ,a) `(id ,b)))
                                         `(id ,x)))))))))

(define-primitive! '- 1 #f
  (simple-compiler
   (lambda args (if (null? (cdr args)) (unop "-" (car args)) (left-fold "-" args))))
  #:value (minus-value "-" (lambda (x) (unop "-" x))))

(define-primitive! '/ 1 #f
  (simple-compiler
   (lambda args (if (null? (cdr args)) (binop "/" (num 1) (car args)) (left-fold "/" args))))
  #:value (minus-value "/" (lambda (x) (binop "/" (num 1) x))))

(define (define-comparison! names op)
  (define-primitive! names 1 #f
    (simple-compiler
     (lambda args
       (match args
         ((a) '(bool #t))
         ((a b) (binop op a b))
         (_ (left-fold "&&" (map (lambda (a b) (binop op a b))
                                 (drop-right args 1)
                                 (cdr args)))))))
    #:dup (lambda (argc) (and (> argc 2) 'all))
    ;; (...xs) => xs.every((x, i) => i === 0 || xs[i - 1] < x)
    #:value (lambda (fresh)
              (let ((xs (fresh "xs")) (x (fresh "x")) (i (fresh "i")))
                `(arrow ((rest ,xs))
                        ((return ,(method `(id ,xs) "every"
                                          (arrow (list x i)
                                                 (binop "||"
                                                        (binop "===" `(id ,i) (num 0))
                                                        (binop op
                                                               `(index (id ,xs) ,(binop "-" `(id ,i) (num 1)))
                                                               `(id ,x))))))))))))

(define-comparison! '(= char=? string=?) "===")

;; x?.length === 0 (#nil and undefined are not empty lists).
(define (empty-list? x)
  (binop "==="
         (match x
           (('array . _) (prop x "length"))
           (_ `(optional-dot ,x "length")))
         (num 0)))

;; Two arrays are never ===, so (eq? x '()) is (null? x).
(define-primitive! '(eq? eqv?) 2 2
  (simple-compiler
   (lambda (a b)
     (match (list a b)
       ((('array) x) (empty-list? x))
       ((x ('array)) (empty-list? x))
       (_ (binop "===" a b)))))
  #:value (lambda (fresh)
            (let ((a (fresh "a")) (b (fresh "b")))
              (arrow (list a b) (binop "===" `(id ,a) `(id ,b))))))

;; Is the JavaScript expression E never an array? (e.g. a string)
(define (atom-expr? e)
  (match e
    ((or ('num _) ('str _) ('bool _) ('null) ('undefined)) #t)
    (('binop (or "+" "-" "*" "/" "%" "**") _ _) #t)
    ;; A variable can be anything, whatever its name.
    ((or ('id _) ('dot _ _)) #f)
    (_ (boolean-expr? e))))

;; equal? compares lists and vectors element by element (with the isEqual
;; helper), unless one of the values is a string, a number...
(define (equal-expr a b)
  (match (list a b)
    ((('array) x) (empty-list? x))
    ((x ('array)) (empty-list? x))
    (_ (if (or (atom-expr? a) (atom-expr? b))
           (binop "===" a b)
           (call (helper-ref 'equal) a b)))))

(define-primitive! 'equal? 2 2
  (simple-compiler equal-expr)
  #:value (lambda (fresh) (helper-ref 'equal))
  #:helpers '(equal))
(define-comparison! '(< char<? string<?) "<")
(define-comparison! '(> char>? string>?) ">")
(define-comparison! '(<= char<=? string<=?) "<=")
(define-comparison! '(>= char>=? string>=?) ">=")

(define-inline not (x) (js-negate (truth x)))
(define-inline 1+ (x) (binop "+" x (num 1)))
(define-inline 1- (x) (binop "-" x (num 1)))
(define-inline zero? (x) (binop "===" x (num 0)))
(define-inline positive? (x) (binop ">" x (num 0)))
(define-inline negative? (x) (binop "<" x (num 0)))
(define-inline odd? (x) (binop "!==" (binop "%" x (num 2)) (num 0)))
(define-inline even? (x) (binop "===" (binop "%" x (num 2)) (num 0)))
(define-inline expt (a b) (binop "**" a b))
(define-inline (exact->inexact inexact->exact exact inexact) (x) x)
(define-inline integer? (x) (call (js-path "Number.isInteger") x))
(define-inline nan? (x) (call (js-path "Number.isNaN") x))

(define (truncate-quotient-expr a b) (call (js-path "Math.trunc") (binop "/" a b)))
(define (truncate-remainder-expr a b) (binop "%" a b))
(define (floor-quotient-expr a b) (call (js-path "Math.floor") (binop "/" a b)))
(define (floor-remainder-expr a b) (binop "%" (binop "+" (binop "%" a b) b) b))

(define-primitive! '(quotient truncate-quotient) 2 2
  (simple-compiler truncate-quotient-expr))

(define-primitive! '(remainder truncate-remainder) 2 2
  (simple-compiler truncate-remainder-expr))

(define-primitive! 'floor-quotient 2 2
  (simple-compiler floor-quotient-expr))

(define-primitive! '(modulo floor-remainder) 2 2
  (simple-compiler floor-remainder-expr)
  #:dup 'all)

;; Two values (an array).
(define-primitive! 'truncate/ 2 2
  (simple-compiler
   (lambda (a b) `(array ,(truncate-quotient-expr a b) ,(truncate-remainder-expr a b))))
  #:dup 'all)

(define-primitive! 'floor/ 2 2
  (simple-compiler
   (lambda (a b) `(array ,(floor-quotient-expr a b) ,(floor-remainder-expr a b))))
  #:dup 'all)

(define-js-function! 'abs "Math.abs")
(define-js-function! 'ceiling "Math.ceil")
(define-js-function! 'floor "Math.floor")
;; Like Scheme, round to even: (round 2.5) => 2 (Math.round(2.5) is 3).
(define (round-to-even x)
  `(ternary ,(binop "===" (call (js-path "Math.abs") (binop "%" x (num 1))) (num 0.5))
            ,(binop "*" (num 2) (call (js-path "Math.round") (binop "/" x (num 2))))
            ,(call (js-path "Math.round") x)))

(define-primitive! 'round 1 1
  (simple-compiler round-to-even)
  #:dup 'all)
(define-js-function! 'truncate "Math.trunc")
(define-js-function! 'max "Math.max")
(define-js-function! 'min "Math.min")
(define-js-function! 'sqrt "Math.sqrt")
(define-js-function! 'exp "Math.exp")
(define-js-function! 'log "Math.log")
(define-js-function! 'sin "Math.sin")
(define-js-function! 'cos "Math.cos")
(define-js-function! 'tan "Math.tan")
(define-js-function! 'asin "Math.asin")
(define-js-function! 'acos "Math.acos")

(define-primitive! 'atan 1 2
  (simple-compiler
   (case-lambda
     ((y) (call (js-path "Math.atan") y))
     ((y x) (call (js-path "Math.atan2") y x)))))

(define-primitive! 'number->string 1 2
  (simple-compiler
   (case-lambda
     ((n) (call '(id "String") n))
     ((n radix) (method n "toString" radix)))))

;; Like Scheme, invalid numbers (including "" and numbers with spaces) are
;; #f: /^[+-]?(\d+\.?\d*|\.\d+)(e[+-]?\d+)?$/i.test(s) ? Number(s) : false
(define decimal-number-regex '(regex "^[+-]?(\\d+\\.?\\d*|\\.\\d+)(e[+-]?\\d+)?$" "i"))

(define (radix-number-regex radix)
  (let ((digits (if (<= radix 10)
                    (format #f "0-~a" (1- radix))
                    (format #f "0-9a-~a" (integer->char (+ (char->integer #\a) radix -11))))))
    `(regex ,(format #f "^[+-]?[~a]+$" digits) "i")))

(define-primitive! 'string->number 1 2
  (lambda (args til fresh)
    (define (decimal s)
      `(ternary ,(method decimal-number-regex "test" s) ,(call '(id "Number") s) (bool #f)))
    (match args
      ((s) (decimal s))
      ((s radix)
       (match (and (const? (second til)) (const-exp (second til)))
         (10 (decimal s))
         ((? (lambda (r) (and (exact-integer? r) (<= 2 r 36))) r)
          `(ternary ,(method (radix-number-regex r) "test" s) ,(call '(id "parseInt") s radix)
                    (bool #f)))
         (_ (call '(id "parseInt") s radix))))))
  #:dup '(0))

;;
;; Types
;;

(define (typeof-check type)
  (lambda (x) (binop "===" (unop "typeof " x) (str type))))

(define-inline number? (x) ((typeof-check "number") x))
(define-inline (string? symbol? keyword? char?) (x) ((typeof-check "string") x))
(define-inline boolean? (x) ((typeof-check "boolean") x))
(define-inline procedure? (x) ((typeof-check "function") x))
(define-inline (list? vector?) (x) (call (js-path "Array.isArray") x))
(define-inline null? (x) (empty-list? x))

(define-primitive! 'pair? 1 1
  (simple-compiler
   (lambda (x)
     (binop "&&"
            (call (js-path "Array.isArray") x)
            (binop ">" (prop x "length") (num 0)))))
  #:dup 'all)

;;
;; Strings and characters (characters are one-character strings)
;;

(define-inline string-length (s) (prop s "length"))
(define-inline string-ref (s i) `(index ,s ,i))
(define-inline (string-upcase char-upcase) (s) (method s "toUpperCase"))
(define-inline (string-downcase char-downcase) (s) (method s "toLowerCase"))
(define-inline string-null? (s) (binop "===" (prop s "length") (num 0)))
(define-inline string-prefix? (prefix s) (method s "startsWith" prefix))
(define-inline string-suffix? (suffix s) (method s "endsWith" suffix))
(define-inline string-split (s c) (method s "split" c))
(define-inline string-trim (s) (method s "trimStart"))
(define-inline string-trim-right (s) (method s "trimEnd"))
(define-inline string-trim-both (s) (method s "trim"))
(define-inline string->list (s) `(array (spread ,s)))
(define-inline list->string (l) (method l "join" (str "")))
(define-inline (string->symbol symbol->string keyword->string) (s) s)
(define-inline char->integer (c) (method c "codePointAt" (num 0)))
(define-inline integer->char (n) (call (js-path "String.fromCodePoint") n))

(define-primitive! 'substring 2 3
  (simple-compiler (lambda (s . args) (apply method s "substring" args))))

(define-primitive! 'string-copy 1 3
  (simple-compiler
   (case-lambda
     ((s) s)
     ((s . args) (apply method s "substring" args)))))

(define-primitive! 'string-join 1 2
  (simple-compiler
   (case-lambda
     ((l) (method l "join" (str " ")))
     ((l delim) (method l "join" delim)))))

(define (string-concatenation args)
  (match args
    (() (str ""))
    ((s) s)
    (_ (left-fold "+" args))))

(define-primitive! '(string-append string) 0 #f
  (simple-compiler (lambda args (string-concatenation args)))
  #:value (lambda (fresh)
            (let ((xs (fresh "xs")))
              `(arrow ((rest ,xs)) ((return ,(method `(id ,xs) "join" (str ""))))))))

;;
;; Lists and vectors (both are arrays)
;;

(define-primitive! '(list vector) 0 #f
  (simple-compiler (lambda args `(array ,@args)))
  #:value (lambda (fresh)
            (let ((xs (fresh "xs")))
              `(arrow ((rest ,xs)) ((return (id ,xs)))))))

(define-inline cons (x l) `(array ,x ,@(array-elements l)))

(define-primitive! 'append 0 #f
  (simple-compiler
   (lambda args
     (match args
       (() '(array))
       ((l) l)
       (_ `(array ,@(append-map array-elements args))))))
  #:value (lambda (fresh)
            (let ((xs (fresh "xs")))
              `(arrow ((rest ,xs))
                      ((return ,(method '(array) "concat" `(spread (id ,xs)))))))))

(define-inline (car first) (l) `(index ,l ,(num 0)))
(define-inline (cadr second) (l) `(index ,l ,(num 1)))
(define-inline (caddr third) (l) `(index ,l ,(num 2)))
(define-inline (cadddr fourth) (l) `(index ,l ,(num 3)))
(define-inline fifth (l) `(index ,l ,(num 4)))
(define-inline caar (l) `(index (index ,l ,(num 0)) ,(num 0)))
(define-inline cdr (l) (method l "slice" (num 1)))
(define-inline cddr (l) (method l "slice" (num 2)))
(define-inline cdddr (l) (method l "slice" (num 3)))
(define-inline cdar (l) (method `(index ,l ,(num 0)) "slice" (num 1)))
(define-inline last (l) (method l "at" (num -1)))
(define-inline (length vector-length) (l) (prop l "length"))
(define-inline (list-ref vector-ref) (l i) `(index ,l ,i))
(define-inline vector-set! (v i x) `(assign (index ,v ,i) ,x))
(define-inline (list-tail drop) (l k) (method l "slice" k))
(define-inline (list-head take) (l k) (method l "slice" (num 0) k))
(define-inline reverse (l) (method (copy-array l) "reverse"))
(define-inline (list-copy vector->list list->vector) (l) (copy-array l))
(define-inline vector-fill! (v x) (method v "fill" x))
(define-inline (memq memv) (x l)
  (match l
    (('array y) (binop "===" x y))
    (_ (method l "includes" x))))

;; member, delete and delete-duplicates use equal?.
(define-primitive! 'member 2 2
  (lambda (args til fresh)
    (match args
      ((x ('array y)) (equal-expr x y))
      ((x l)
       (if (atom-expr? x)
           (method l "includes" x)
           (let ((y (fresh "y")))
             (method l "some" (arrow (list y) (equal-expr `(id ,y) x))))))))
  #:dup '(0)
  #:helpers '(equal))

;; l.filter((x, i) => l.findIndex((y) => isEqual(x, y)) === i)
(define-primitive! 'delete-duplicates 1 1
  (lambda (args til fresh)
    (match args
      ((l)
       (let ((x (fresh "x")) (i (fresh "i")) (y (fresh "y")))
         (method l "filter"
                 (arrow (list x i)
                        (binop "==="
                               (method l "findIndex" (arrow (list y) (equal-expr `(id ,x) `(id ,y))))
                               `(id ,i))))))))
  #:dup '(0)
  #:helpers '(equal))

(define-primitive! 'vector-copy 1 3
  (simple-compiler
   (case-lambda
     ((v) `(array (spread ,v)))
     ((v . args) (apply method v "slice" args)))))

(define-primitive! '(make-vector make-list) 1 2
  (simple-compiler
   (lambda (n . fill) (apply method `(new (id "Array") ,n) "fill" fill))))

(define-primitive! 'iota 1 3
  (lambda (args til fresh)
    (let* ((i (fresh "i"))
           (value (match args
                    ((n) `(id ,i))
                    ((n start) (binop "+" start `(id ,i)))
                    ((n start step) (binop "+" start (binop "*" `(id ,i) step))))))
      (call (js-path "Array.from")
            `(object ("length" . ,(car args)))
            `(arrow ((param ,(fresh "_")) (param ,i)) ((return ,value))))))
  #:dup '(1 2))

;; Methods that call a predicate: filter, some...
(define (define-predicate-method! name js-method)
  (define-primitive! name 2 2
    (lambda (args til fresh)
      (match args
        ((f l) (method l js-method (predicate-callback f (first til) fresh)))))
    #:dup '(0)))

;; With several lists, (map f l1 l2 ...) => l1.map((x, i) => f(x, l2[i], ...)).
(define (lists-callback f f-til ls fresh)
  (match ls
    (() (callback f f-til 1 fresh))
    (_ (let ((x (fresh "x")) (i (fresh "i")))
         (arrow (list x i)
                (call-callback f f-til
                               (cons `(id ,x) (map (lambda (l) `(index ,l (id ,i))) ls))
                               fresh))))))

;; The function and the other lists are used for each element.
(define (lists-dup argc)
  (cons 0 (iota (- argc 2) 2)))

;; (f, l, ...ls) => l.map((x, i) => f(x, ...ls.map((m) => m[i])))
(define (lists-value js-method)
  (lambda (fresh)
    (let ((f (fresh "f")) (l (fresh "l")) (ls (fresh "ls"))
          (x (fresh "x")) (i (fresh "i")) (m (fresh "m")))
      `(arrow ((param ,f) (param ,l) (rest ,ls))
              ((return ,(method `(id ,l) js-method
                                (arrow (list x i)
                                       `(call (id ,f) (id ,x)
                                              (spread ,(method `(id ,ls) "map"
                                                               (arrow (list m) `(index (id ,m) (id ,i))))))))))))))

(define-primitive! 'map 2 #f
  (lambda (args til fresh)
    (match args
      ((f l . ls) (method l "map" (lists-callback f (first til) ls fresh)))))
  #:dup lists-dup
  #:value (lists-value "map"))

(define-primitive! 'for-each 2 #f
  (lambda (args til fresh)
    (match args
      ((f l . ls)
       ;; forEach doesn't wait for the promises returned by async functions.
       (match f
         (('async . _)
          (compile-warning (current-location)
                           "the function given to for-each uses js-await, but for-each doesn't wait for it (use a loop, or map and Promise.all)"))
         (_ #t))
       (method l "forEach" (lists-callback f (first til) ls fresh)))))
  #:dup lists-dup
  #:value (lists-value "forEach"))
(define-predicate-method! 'filter "filter")
(define-predicate-method! 'any "some")
(define-predicate-method! 'every "every")
(define-predicate-method! 'find "find")

(define-primitive! 'remove 2 2
  (lambda (args til fresh)
    (match args
      ((f l)
       (let ((x (fresh "x")))
         (method l "filter"
                 (arrow (list x) (js-negate (truth (call-callback f (first til) `((id ,x)) fresh)))))))))
  #:dup '(0))

(define-primitive! 'count 2 2
  (lambda (args til fresh)
    (match args
      ((f l) (prop (method l "filter" (predicate-callback f (first til) fresh)) "length"))))
  #:dup '(0))

(define-primitive! 'delete 2 2
  (lambda (args til fresh)
    (match args
      ((x l)
       (let ((y (fresh "y")))
         (method l "filter" (arrow (list y) (js-negate (equal-expr `(id ,y) x))))))))
  #:dup '(0)
  #:helpers '(equal))

;; SRFI-1 fold calls (kons element accumulator).
(define-primitive! 'fold 3 3
  (lambda (args til fresh)
    (match args
      ((kons knil l) (method l "reduce" (swapped-callback kons (first til) fresh) knil))))
  #:dup '(0))

(define-primitive! 'fold-right 3 3
  (lambda (args til fresh)
    (match args
      ((kons knil l) (method l "reduceRight" (swapped-callback kons (first til) fresh) knil))))
  #:dup '(0))

(define-primitive! 'reduce 3 3
  (lambda (args til fresh)
    (match args
      ((f ridentity l)
       `(ternary ,(binop "===" (prop l "length") (num 0))
                 ,ridentity
                 ,(method l "reduce" (swapped-callback f (first til) fresh))))))
  #:dup '(1 2))

(define-primitive! 'sort 2 2
  (lambda (args til fresh)
    (match args
      ((l less)
       (let* ((a (fresh "a"))
              (b (fresh "b"))
              (less? (lambda (x y)
                       (truth (call-callback less (second til) (list `(id ,x) `(id ,y)) fresh)))))
         (method (copy-array l) "sort"
                 (arrow (list a b)
                        `(ternary ,(less? a b) ,(num -1)
                                  (ternary ,(less? b a) ,(num 1) ,(num 0)))))))))
  #:dup '(1))

;;
;; Object properties. (assoc-ref obj key) is obj[key], so code that reads
;; objects with assoc-ref can be shared with Guile code, where they would be
;; association lists.
;;

(define-primitive! '(assoc-ref assq-ref assv-ref) 2 2
  (lambda (args til fresh)
    (match args
      ((obj key) (member-access obj key (second til))))))

;;
;; Hash tables (Map)
;;

(define-primitive! 'make-hash-table 0 1
  (simple-compiler (lambda _ '(new (id "Map")))))

(define-primitive! 'hash-ref 2 3
  (simple-compiler
   (case-lambda
     ((h k) (method h "get" k))
     ((h k default) `(ternary ,(method h "has" k) ,(method h "get" k) ,default))))
  #:dup (lambda (argc) (and (= argc 3) '(0 1))))

(define-inline hash-set! (h k v) (method h "set" k v))
(define-inline hash-remove! (h k) (method h "delete" k))
(define-inline hash-clear! (h) (method h "clear"))

(define-primitive! 'hash-for-each 2 2
  (lambda (args til fresh)
    (match args
      ((f h) (method h "forEach" (swapped-callback f (first til) fresh)))))
  #:dup '(0))

(define-primitive! 'hash-map->list 2 2
  (lambda (args til fresh)
    (match args
      ((f h)
       (match (and (function-with-arity? f 2) f)
         (('arrow (('param k) ('param v)) body)
          (call (js-path "Array.from") h `(arrow ((param-array ,k ,v)) ,body)))
         (_
          (let ((k (fresh "k")) (v (fresh "v")))
            (call (js-path "Array.from") h
                  `(arrow ((param-array ,k ,v))
                          ((return ,(call-callback f (first til) `((id ,k) (id ,v)) fresh)))))))))))
  #:dup '(0))

;;
;; JavaScript interoperability
;;

;; obj.a.b for the KEYS (compiled, and KEYS-TIL in Tree-IL) a and b.
(define (member-path obj keys keys-til)
  (fold (lambda (key key-til obj) (member-access obj key key-til))
        obj keys keys-til))

;; (js-ref obj 'a 'b) => obj.a.b
(define-primitive! 'js-ref 2 #f
  (lambda (args til fresh)
    (member-path (car args) (cdr args) (cdr til))))

;; (js-set! obj 'a 'b value) => obj.a.b = value
(define-primitive! 'js-set! 3 #f
  (lambda (args til fresh)
    `(assign ,(member-path (car args) (drop-right (cdr args) 1) (drop-right (cdr til) 1))
             ,(last args))))

;; (js-invoke obj 'method arg ...) => obj.method(arg, ...)
(define-primitive! 'js-invoke 2 #f
  (lambda (args til fresh)
    `(call ,(member-access (first args) (second args) (second til))
           ,@(cddr args))))

;; (js-new Date arg ...) => new Date(arg, ...)
;; (js-new "google.maps.Map" arg ...) => new google.maps.Map(arg, ...)
(define-primitive! 'js-new 1 #f
  (lambda (args til fresh)
    (let ((name (constant-name (first til))))
      `(new ,(if name (js-path name) (first args)) ,@(cdr args)))))

;; (js-obj #:name "Bob" 'age 32) => { name: "Bob", age: 32 }
(define-primitive! 'js-obj 0 #f
  (lambda (args til fresh)
    (let lp ((args args) (til til) (entries '()))
      (match args
        (() `(object ,@(reverse entries)))
        ((key value . rest)
         (lp rest (cddr til)
             (cons (cons (or (constant-name (car til)) `(computed ,key)) value)
                   entries)))
        (_ (compile-error "js-obj needs an even number of arguments"))))))

;;
;; Multiple values are arrays (a single value is just the value).
;;

(define-primitive! 'values 0 #f
  (simple-compiler
   (lambda args
     (match args
       ((x) x)
       (_ `(array ,@args)))))
  #:value (lambda (fresh)
            (let ((xs (fresh "xs")))
              `(arrow ((rest ,xs))
                      ((return (ternary (binop "===" (dot (id ,xs) "length") (num 1))
                                        (index (id ,xs) (num 0))
                                        (id ,xs))))))))

;;
;; Errors
;;

(define-inline error-object? (x) (binop "instanceof" x '(id "Error")))
(define-inline error-object-message (x) (prop x "message"))

(define-inline js-typeof (x) (unop "typeof " x))
(define-inline js-instanceof (x type) (binop "instanceof" x type))

(define-primitive! 'js-delete! 2 2
  (lambda (args til fresh)
    (unop "delete " (member-access (first args) (second args) (second til)))))
