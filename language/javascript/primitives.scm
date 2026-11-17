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
;; arrays, symbols and characters are strings, hash tables are Maps and
;; quoted association lists are objects.
;;
;; Each primitive has a compiler procedure that receives:
;;
;;   ARGS   the arguments already compiled to JavaScript expressions
;;   TIL    the original Tree-IL arguments (an element can be #f when the
;;          primitive is used as a first-class value)
;;   FRESH  a procedure that returns a fresh JavaScript variable name
;;
;; Arguments listed in DUP are used more than once in the result, so the
;; compiler makes sure they are simple (variables or literals) before
;; calling the primitive compiler.

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
            primitive?
            primitive-name
            primitive-min-args
            primitive-max-args
            primitive-dup-indices
            primitive-compiler
            primitive-value-builder
            js-path
            call-function
            js-negate
            substitute
            constant-name))

(define-record-type <primitive>
  (make-primitive name min max dup compiler value)
  primitive?
  (name primitive-name)
  (min primitive-min-args)
  (max primitive-max-args)              ; #f means variadic
  (dup primitive-dup)                   ; #f, 'all, list or procedure
  (compiler primitive-compiler)
  (value primitive-value-builder))      ; #f or (lambda (fresh) ...)

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

(define* (define-primitive! names min max compiler #:key dup value)
  (for-each (lambda (name)
              (hashq-set! primitives name
                          (make-primitive name min max dup compiler value)))
            (if (list? names) names (list names))))

;;
;; Helpers
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
      (((or 'arrow 'function) . _) (if in-functions? (walk (cdr e)) 0))
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
                                     (((or 'arrow 'function 'assign) . _) #t)
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
    (('unop "!" x) x)
    (('binop "===" a b) `(binop "!==" ,a ,b))
    (('binop "!==" a b) `(binop "===" ,a ,b))
    (_ `(unop "!" ,e))))

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

(define-primitive! '+ 0 #f
  (simple-compiler
   (lambda args (if (null? args) (num 0) (left-fold "+" args))))
  #:value (lambda (fresh)
            (let ((xs (fresh "xs")) (a (fresh "a")) (b (fresh "b")))
              `(arrow ((rest ,xs))
                      ((return ,(method `(id ,xs) "reduce"
                                        (arrow (list a b) (binop "+" `(id ,a) `(id ,b)))
                                        (num 0))))))))

(define-primitive! '* 0 #f
  (simple-compiler
   (lambda args (if (null? args) (num 1) (left-fold "*" args))))
  #:value (lambda (fresh)
            (let ((xs (fresh "xs")) (a (fresh "a")) (b (fresh "b")))
              `(arrow ((rest ,xs))
                      ((return ,(method `(id ,xs) "reduce"
                                        (arrow (list a b) (binop "*" `(id ,a) `(id ,b)))
                                        (num 1))))))))

(define-primitive! '- 1 #f
  (simple-compiler
   (lambda args (if (null? (cdr args)) (unop "-" (car args)) (left-fold "-" args))))
  #:value (lambda (fresh)
            (let ((a (fresh "a")) (b (fresh "b")))
              (arrow (list a b) (binop "-" `(id ,a) `(id ,b))))))

(define-primitive! '/ 1 #f
  (simple-compiler
   (lambda args (if (null? (cdr args)) (binop "/" (num 1) (car args)) (left-fold "/" args))))
  #:value (lambda (fresh)
            (let ((a (fresh "a")) (b (fresh "b")))
              (arrow (list a b) (binop "/" `(id ,a) `(id ,b))))))

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
    #:value (lambda (fresh)
              (let ((a (fresh "a")) (b (fresh "b")))
                (arrow (list a b) (binop op `(id ,a) `(id ,b)))))))

(define-comparison! '(= eq? eqv? equal? char=? string=?) "===")
(define-comparison! '(< char<? string<?) "<")
(define-comparison! '(> char>? string>?) ">")
(define-comparison! '(<= char<=? string<=?) "<=")
(define-comparison! '(>= char>=? string>=?) ">=")

(define-inline not (x) (js-negate x))
(define-inline 1+ (x) (binop "+" x (num 1)))
(define-inline 1- (x) (binop "-" x (num 1)))
(define-inline zero? (x) (binop "===" x (num 0)))
(define-inline positive? (x) (binop ">" x (num 0)))
(define-inline negative? (x) (binop "<" x (num 0)))
(define-inline odd? (x) (binop "!==" (binop "%" x (num 2)) (num 0)))
(define-inline even? (x) (binop "===" (binop "%" x (num 2)) (num 0)))
(define-inline quotient (a b) (call (js-path "Math.trunc") (binop "/" a b)))
(define-inline remainder (a b) (binop "%" a b))
(define-inline expt (a b) (binop "**" a b))
(define-inline (exact->inexact inexact->exact exact inexact) (x) x)
(define-inline number? (x) (binop "===" (unop "typeof " x) (str "number")))
(define-inline integer? (x) (call (js-path "Number.isInteger") x))
(define-inline nan? (x) (call (js-path "Number.isNaN") x))

(define-primitive! 'modulo 2 2
  (simple-compiler
   (lambda (a b) (binop "%" (binop "+" (binop "%" a b) b) b)))
  #:dup 'all)

(define-js-function! 'abs "Math.abs")
(define-js-function! 'ceiling "Math.ceil")
(define-js-function! 'floor "Math.floor")
(define-js-function! 'round "Math.round")
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

(define-primitive! 'string->number 1 2
  (simple-compiler
   (case-lambda
     ((s) (call '(id "Number") s))
     ((s radix) (call '(id "parseInt") s radix)))))

;;
;; Types
;;

(define (typeof-check type)
  (lambda (x) (binop "===" (unop "typeof " x) (str type))))

(define-inline (string? symbol? keyword? char?) (x) ((typeof-check "string") x))
(define-inline boolean? (x) ((typeof-check "boolean") x))
(define-inline procedure? (x) ((typeof-check "function") x))
(define-inline (list? vector?) (x) (call (js-path "Array.isArray") x))
(define-inline null? (x) (binop "===" (prop x "length") (num 0)))

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
(define-inline (memq memv member) (x l)
  (match l
    (('array y) (binop "===" x y))
    (_ (method l "includes" x))))
(define-inline delete-duplicates (l)
  `(array (spread (new (id "Set") ,l))))

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

(define (define-callback-method! name js-method)
  (define-primitive! name 2 2
    (lambda (args til fresh)
      (match args
        ((f l) (method l js-method (callback f (first til) 1 fresh)))))
    #:dup '(0)))

(define-callback-method! 'map "map")

(define-primitive! 'for-each 2 2
  (lambda (args til fresh)
    (match args
      ((f l)
       ;; forEach doesn't wait for the promises returned by async functions.
       (match f
         (('async . _)
          (compile-warning (current-location)
                           "the function given to for-each uses js-await, but for-each doesn't wait for it (use a loop, or map and Promise.all)"))
         (_ #t))
       (method l "forEach" (callback f (first til) 1 fresh)))))
  #:dup '(0))
(define-callback-method! 'filter "filter")
(define-callback-method! 'any "some")
(define-callback-method! 'every "every")
(define-callback-method! 'find "find")

(define-primitive! 'remove 2 2
  (lambda (args til fresh)
    (match args
      ((f l)
       (let ((x (fresh "x")))
         (method l "filter"
                 (arrow (list x) (js-negate (call-callback f (first til) `((id ,x)) fresh))))))))
  #:dup '(0))

(define-primitive! 'count 2 2
  (lambda (args til fresh)
    (match args
      ((f l) (prop (method l "filter" (callback f (first til) 1 fresh)) "length"))))
  #:dup '(0))

(define-primitive! 'delete 2 2
  (lambda (args til fresh)
    (match args
      ((x l)
       (let ((y (fresh "y")))
         (method l "filter" (arrow (list y) (binop "!==" `(id ,y) x)))))))
  #:dup '(0))

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
                       (call-callback less (second til) (list `(id ,x) `(id ,y)) fresh))))
         (method (copy-array l) "sort"
                 (arrow (list a b)
                        `(ternary ,(less? a b) ,(num -1)
                                  (ternary ,(less? b a) ,(num 1) ,(num 0)))))))))
  #:dup '(1))

;;
;; Association lists (quoted association lists are objects)
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

;; (js-ref obj 'a 'b) => obj.a.b
(define-primitive! 'js-ref 2 #f
  (lambda (args til fresh)
    (fold (lambda (key key-til obj) (member-access obj key key-til))
          (car args) (cdr args) (cdr til))))

;; (js-set! obj 'a 'b value) => obj.a.b = value
(define-primitive! 'js-set! 3 #f
  (lambda (args til fresh)
    (let ((keys (drop-right (cdr args) 1))
          (keys-til (drop-right (cdr til) 1)))
      `(assign ,(fold (lambda (key key-til obj) (member-access obj key key-til))
                      (car args) keys keys-til)
               ,(last args)))))

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
