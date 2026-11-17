;;; (language javascript print) --- GuileScript.

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

;; Printer for the small JavaScript AST produced by the Tree-IL compiler.
;;
;; Expressions:
;;
;;   (num N) (str S) (bool B) (null) (undefined) (id NAME) (this)
;;   (array ELT ...)            ELT can be (spread E)
;;   (object (KEY . E) ...)     KEY is a string or (computed E); an entry
;;                              can also be (spread E)
;;   (call F ARG ...)           ARG can be (spread E)
;;   (new F ARG ...)
;;   (dot E NAME) (index E E)
;;   (binop OP A B) (unop OP A) (ternary T A B) (assign TARGET E)
;;   (function NAME PARAMS STMTS) (arrow PARAMS STMTS) (async ARROW)
;;
;; Parameters:
;;
;;   (param NAME) (param NAME DEFAULT) (rest NAME) (param-array NAME ...)
;;   (options ((KEY NAME DEFAULT) ...))   DEFAULT can be #f
;;
;; Statements:
;;
;;   (expr-stmt E) (decl KIND NAME E) (fun-decl NAME PARAMS STMTS)
;;   (async-fun-decl NAME PARAMS STMTS) (decl-array KIND NAMES REST E)
;;   (class NAME ((constructor PARAMS STMTS)))
;;   (return E) (if T STMTS STMTS) (while LABEL STMTS) (break LABEL)
;;   (continue LABEL) (throw E) (try STMTS NAME STMTS) (block STMTS) (export STMT)
;;   (export-names NAMES) (import FROM DEFAULT NAMESPACE NAMES)
;;
;; E, LABEL, the else STMTS and DEFAULT can be #f. The label of a while loop
;; is a pair (NAME . USED?) and it is only printed if USED? is true.

;;; Code:

(define-module (language javascript print)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (language javascript names)
  #:export (print-js
            print-js-expression
            js-string-literal))

(define max-line-width 80)

(define (indent-string level)
  (make-string (* 2 level) #\space))

;;
;; Literals
;;

(define (js-string-literal s)
  (call-with-output-string
    (lambda (port)
      (write-char #\" port)
      (string-for-each
       (lambda (c)
         (case c
           ((#\\) (display "\\\\" port))
           ((#\") (display "\\\"" port))
           ((#\newline) (display "\\n" port))
           ((#\return) (display "\\r" port))
           ((#\tab) (display "\\t" port))
           (else
            (let ((n (char->integer c)))
              (if (or (< n #x20) (= n #x7f) (= n #x2028) (= n #x2029))
                  (format port "\\u~4,'0x" n)
                  (write-char c port))))))
       s)
      (write-char #\" port))))

(define (js-number n)
  (cond
   ((and (exact? n) (integer? n)) (number->string n))
   ((exact? n) (js-number (exact->inexact n)))
   ((nan? n) "NaN")
   ((inf? n) (if (positive? n) "Infinity" "-Infinity"))
   ((and (integer? n) (< (abs n) 1e21)) (number->string (inexact->exact n)))
   (else (number->string n))))

(define (property-key key indent)
  (match key
    (('computed e) (string-append "[" (expr e 2 indent) "]"))
    ((? js-identifier-name?) key)
    (_ (js-string-literal key))))

;;
;; Precedence
;;

(define (binop-precedence op)
  (match op
    ("||" 4) ("&&" 5) ("|" 6) ("^" 7) ("&" 8)
    ((or "==" "!=" "===" "!==") 9)
    ((or "<" ">" "<=" ">=" "instanceof" "in") 10)
    ((or "<<" ">>" ">>>") 11)
    ((or "+" "-") 12)
    ((or "*" "/" "%") 13)
    ("**" 14)))

(define (precedence e)
  (match e
    (('num n) (if (or (negative? n) (eqv? n -0.0)) 15 18))
    (('assign . _) 2)
    (('arrow . _) 2)
    (('async . _) 2)
    (('ternary . _) 3)
    (('binop op . _) (binop-precedence op))
    (('unop . _) 15)
    ((or ('call . _) ('new . _) ('dot . _) ('index . _)) 17)
    (_ 18)))

;;
;; Expressions
;;

(define (expr e prec indent)
  (let ((s (expr* e indent)))
    (if (< (precedence e) prec)
        (string-append "(" s ")")
        s)))

(define (element e indent)
  (match e
    (('spread x) (string-append "..." (expr x 2 indent)))
    (_ (expr e 2 indent))))

(define (comma-list items)
  (string-join items ", "))

;; Print a bracketed list of items, on a single line if it fits.
(define (bracketed open close items indent pad?)
  (cond
   ((null? items) (string-append open close))
   ((and (not (any (lambda (s) (string-index s #\newline)) items))
         (< (+ (* 2 indent) (string-length (comma-list items)) 4)
            max-line-width))
    (if pad?
        (string-append open " " (comma-list items) " " close)
        (string-append open (comma-list items) close)))
   (else
    (string-append open "\n"
                   (string-join (map (lambda (s)
                                       (string-append (indent-string (+ indent 1)) s))
                                     items)
                                ",\n")
                   "\n" (indent-string indent) close))))

(define (member-object e indent)
  (match e
    (('num n) (string-append "(" (expr* e indent) ")"))
    (_ (expr e 17 indent))))

(define (callee e indent)
  (match e
    (('function . _) (string-append "(" (expr* e indent) ")"))
    (_ (expr e 17 indent))))

(define (new-callee e indent)
  (match e
    ((or ('call . _) ('function . _)) (string-append "(" (expr* e indent) ")"))
    (_ (expr e 17 indent))))

(define (arguments args indent)
  (string-append "(" (comma-list (map (lambda (a) (element a indent)) args)) ")"))

(define (expr* e indent)
  (match e
    (('num n) (js-number n))
    (('str s) (js-string-literal s))
    (('bool b) (if b "true" "false"))
    (('null) "null")
    (('undefined) "undefined")
    (('this) "this")
    (('id name) name)
    (('array . elts)
     (bracketed "[" "]" (map (lambda (x) (element x (+ indent 1))) elts) indent #f))
    (('object . entries)
     (bracketed "{" "}"
                (map (lambda (entry)
                       (match entry
                         (('spread x) (string-append "..." (expr x 2 (+ indent 1))))
                         ((key . value)
                          (if (equal? value `(id ,key))
                              key       ; shorthand: { name }
                              (string-append (property-key key (+ indent 1)) ": "
                                             (expr value 2 (+ indent 1)))))))
                     entries)
                indent #t))
    (('call f . args)
     (string-append (callee f indent) (arguments args indent)))
    (('new f . args)
     (string-append "new " (new-callee f indent) (arguments args indent)))
    (('dot obj name)
     (string-append (member-object obj indent) "." name))
    (('index obj key)
     (string-append (member-object obj indent) "[" (expr key 0 indent) "]"))
    (('binop op a b)
     (let ((p (binop-precedence op)))
       (if (string=? op "**")
           (string-append (expr a 16 indent) " ** " (expr b p indent))
           ;; && and || are associative: a || (b || c) => a || b || c
           (let ((right-prec (if (and (member op '("&&" "||"))
                                      (match b (('binop (? (cut string=? <> op)) . _) #t) (_ #f)))
                                 p
                                 (+ p 1))))
             (string-append (expr a p indent) " " op " " (expr b right-prec indent))))))
    (('unop op a)
     (let ((operand (expr a 15 indent)))
       (string-append op
                      (if (and (member op '("-" "+"))
                               (string-prefix? op operand))
                          " "
                          "")
                      operand)))
    (('ternary t a b)
     (string-append (expr t 4 indent) " ? " (expr a 2 indent) " : " (expr b 2 indent)))
    (('assign target value)
     (string-append (expr target 17 indent) " = " (expr value 2 indent)))
    (('function name params body)
     (string-append "function" (if name (string-append " " name) "")
                    (parameters params indent) " " (block body indent)))
    (('arrow params body)
     (string-append (parameters params indent) " => " (arrow-body body indent)))
    (('async f)
     (string-append "async " (expr* f indent)))
    (_ (error "GuileScript: unknown JavaScript expression" e))))

(define (arrow-body body indent)
  (match body
    ((('return (? identity e)))
     (match e
       (('object . _) (string-append "(" (expr* e indent) ")"))
       (_ (expr e 2 indent))))
    (_ (block body indent))))

(define (parameters params indent)
  (string-append
   "("
   (comma-list
    (map (match-lambda
           (('param name) name)
           (('param name default) (string-append name " = " (expr default 2 indent)))
           (('rest name) (string-append "..." name))
           (('param-array . names) (string-append "[" (comma-list names) "]"))
           (('options entries)
            (string-append
             (bracketed "{" "}"
                        (map (match-lambda
                               ((key name default)
                                (string-append
                                 (if (string=? key name)
                                     name
                                     (string-append (property-key key indent) ": " name))
                                 (if default
                                     (string-append " = " (expr default 2 indent))
                                     ""))))
                             entries)
                        indent #t)
             " = {}")))
         params))
   ")"))

;;
;; Statements
;;

(define (block stmts indent)
  (if (null? stmts)
      "{}"
      (string-append "{\n" (statements stmts (+ indent 1)) (indent-string indent) "}")))

(define (statements stmts indent)
  (string-concatenate (map (lambda (s) (statement s indent)) stmts)))

;; An expression statement can't start with "function", "{" or "class".
(define (expression-statement e indent)
  (let ((s (expr e 0 indent)))
    (if (or (string-prefix? "function" s)
            (string-prefix? "{" s)
            (string-prefix? "class" s)
            (string-prefix? "let[" s))
        (string-append "(" s ")")
        s)))

(define (statement s indent)
  (let ((pad (indent-string indent)))
    (match s
      (('expr-stmt e)
       (string-append pad (expression-statement e indent) ";\n"))
      (('decl-array kind names rest value)
       (string-append pad kind " ["
                      (comma-list (append names
                                          (if rest (list (string-append "..." rest)) '())))
                      "] = " (expr value 2 indent) ";\n"))
      (('decl kind name value)
       (string-append pad kind " " name
                      (if value (string-append " = " (expr value 2 indent)) "")
                      ";\n"))
      (('fun-decl name params body)
       (string-append pad "function " name (parameters params indent) " "
                      (block body indent) "\n"))
      (('class name members)
       (string-append pad "class " name " {\n"
                      (string-concatenate
                       (map (match-lambda
                              (('constructor params body)
                               (string-append (indent-string (+ indent 1)) "constructor"
                                              (parameters params (+ indent 1)) " "
                                              (block body (+ indent 1)) "\n")))
                            members))
                      pad "}\n"))
      (('async-fun-decl name params body)
       (string-append pad "async function " name (parameters params indent) " "
                      (block body indent) "\n"))
      (('return value)
       (string-append pad "return" (if value (string-append " " (expr value 0 indent)) "")
                      ";\n"))
      (('if _ _ _)
       (string-append pad (if-statement s indent) "\n"))
      (('while label body)
       (string-append pad
                      (if (and label (cdr label)) (string-append (car label) ": ") "")
                      "while (true) " (block body indent) "\n"))
      (('break label)
       (string-append pad "break" (if label (string-append " " label) "") ";\n"))
      (('continue label)
       (string-append pad "continue" (if label (string-append " " label) "") ";\n"))
      (('throw e)
       (string-append pad "throw " (expr e 0 indent) ";\n"))
      (('try body name handler)
       (string-append pad "try " (block body indent)
                      " catch (" name ") " (block handler indent) "\n"))
      (('block body)
       (string-append pad (block body indent) "\n"))
      (('export stmt)
       (string-append pad "export " (string-trim-both (statement stmt indent)) "\n"))
      (('export-names names)
       (string-append pad "export " (bracketed "{" "}" names indent #t) ";\n"))
      (('import from default namespace names)
       (string-append
        pad "import "
        (if (or default namespace (pair? names))
            (string-append
             (comma-list
              (filter identity
                      (list default
                            (and namespace (string-append "* as " namespace))
                            (and (pair? names) (bracketed "{" "}" names indent #t)))))
             " from ")
            "")
        (js-string-literal from) ";\n"))
      (_ (error "GuileScript: unknown JavaScript statement" s)))))

(define (if-statement s indent)
  (match s
    (('if test then else)
     (string-append "if (" (expr test 0 indent) ") " (block then indent)
                    (match else
                      (#f "")
                      (((and ('if . _) nested))
                       (string-append " else " (if-statement nested indent)))
                      (_ (string-append " else " (block else indent))))))))

;;
;; Programs
;;

(define (multi-line? s)
  (let ((i (string-index s #\newline)))
    (and i (< i (- (string-length s) 1)))))

;; Print a list of top-level statements, separating multi-line statements
;; with blank lines.
(define (print-js stmts)
  (let lp ((stmts stmts) (prev #f) (acc '()))
    (if (null? stmts)
        (string-concatenate-reverse acc)
        (let ((s (statement (car stmts) 0)))
          (lp (cdr stmts) s
              (cons s (if (and prev (or (multi-line? prev) (multi-line? s)))
                          (cons "\n" acc)
                          acc)))))))

(define (print-js-expression e)
  (expr e 0 0))
