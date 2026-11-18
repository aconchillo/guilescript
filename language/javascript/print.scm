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
;;   (regex PATTERN FLAGS)
;;   (array ELT ...)            ELT can be (spread E)
;;   (object (KEY . E) ...)     KEY is a string or (computed E)
;;   (call F ARG ...)           ARG can be (spread E)
;;   (new F ARG ...)
;;   (dot E NAME) (optional-dot E NAME) (index E E)
;;   (binop OP A B) (unop OP A) (ternary T A B) (assign TARGET E)
;;   (arrow PARAMS STMTS) (async ARROW)
;;
;; Parameters:
;;
;;   (param NAME) (param NAME DEFAULT) (rest NAME) (param-array NAME ...)
;;   (options ((KEY NAME DEFAULT) ...))   DEFAULT can be #f
;;
;; Statements:
;;
;;   (expr-stmt E) (decl KIND NAME E) (decl-array KIND NAMES REST E)
;;   (fun-decl NAME PARAMS STMTS) (async FUN-DECL)
;;   (class NAME ((constructor PARAMS STMTS)))
;;   (return E) (if T STMTS STMTS) (while LABEL STMTS) (break LABEL)
;;   (continue LABEL) (throw E) (try STMTS NAME STMTS) (export STMT)
;;   (export-names NAMES) (import FROM DEFAULT NAMESPACE NAMES)
;;
;; E, LABEL, the else STMTS and DEFAULT can be #f.

;;; Code:

(define-module (language javascript print)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (language javascript names)
  #:export (print-js
            print-js-with-locations
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
              (if (or (< n #x20) (= n #x7f) (= n #x2028) (= n #x2029)
                      ;; Used as markers by print-js-with-locations.
                      (= n #xe000) (= n #xe001))
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
    ((or "||" "??") 4) ("&&" 5) ("|" 6) ("^" 7) ("&" 8)
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
    ((or ('call . _) ('new . _) ('dot . _) ('optional-dot . _) ('index . _)) 17)
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

;; ITEMS between OPEN and CLOSE, one per line.
(define (one-per-line open close items indent)
  (string-append open "\n"
                 (string-join (map (lambda (s) (string-append (indent-string (+ indent 1)) s))
                                   items)
                              ",\n")
                 "\n" (indent-string indent) close))

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
   (else (one-per-line open close items indent))))

(define (member-object e indent)
  (match e
    (('num n) (string-append "(" (expr* e indent) ")"))
    (_ (expr e 17 indent))))

;; new f()() is (new f())(), so the callee of new can't be a call.
(define (new-callee e indent)
  (match e
    (('call . _) (string-append "(" (expr* e indent) ")"))
    (_ (expr e 17 indent))))

;; Remove one level of indentation from the lines (except the first one) of
;; S, which was printed one level deeper than needed.
(define (dedent s)
  (string-join (let ((lines (string-split s #\newline)))
                 (cons (car lines)
                       (map (lambda (line)
                              (if (string-prefix? "  " line) (substring line 2) line))
                            (cdr lines))))
               "\n"))

;; The arguments of a call, after the callee (PREFIX characters). They are on
;; the same line if they fit, or if only the last one (e.g. a function) spans
;; several lines. Otherwise, there's one argument per line.
(define (arguments args indent prefix)
  (let* ((items (map (lambda (a) (element a (+ indent 1))) args))
         (multi-line (filter (lambda (s) (string-index s #\newline)) items))
         (inline (comma-list items)))
    (cond
     ((and (null? multi-line)
           (<= (+ (* 2 indent) prefix (string-length inline) 2) max-line-width))
      (string-append "(" inline ")"))
     ((and (pair? items)
           (equal? multi-line (list (last items)))
           (<= (+ (* 2 indent) prefix
                  (string-length (comma-list (append (drop-right items 1)
                                                     (list (first-line (last items))))))
                  1)
               max-line-width))
      (string-append "(" (comma-list (append (drop-right items 1)
                                              (list (dedent (last items)))))
                     ")"))
     (else (one-per-line "(" ")" items indent)))))

(define (last-line s)
  (match (string-rindex s #\newline)
    (#f s)
    (i (substring s (+ i 1)))))

(define (first-line s)
  (match (string-index s #\newline)
    (#f s)
    (i (substring s 0 i))))

(define (expr* e indent)
  (match e
    (('num n) (js-number n))
    (('str s) (js-string-literal s))
    (('regex pattern flags) (string-append "/" pattern "/" flags))
    (('bool b) (if b "true" "false"))
    (('null) "null")
    (('undefined) "undefined")
    (('this) "this")
    (('id name) name)
    (('array . elts)
     (bracketed "[" "]" (map (lambda (x) (element x (+ indent 1))) elts) indent #f))
    (('object . entries)
     (bracketed "{" "}"
                (map (match-lambda
                       ((key . value)
                        (if (equal? value `(id ,key))
                            key         ; shorthand: { name }
                            (string-append (property-key key (+ indent 1)) ": "
                                           (expr value 2 (+ indent 1))))))
                     entries)
                indent #t))
    (('call f . args)
     (let ((f (expr f 17 indent)))
       (string-append f (arguments args indent (string-length (last-line f))))))
    (('new f . args)
     (let ((f (string-append "new " (new-callee f indent))))
       (string-append f (arguments args indent (string-length (last-line f))))))
    (('dot obj name)
     (string-append (member-object obj indent) "." name))
    (('optional-dot obj name)
     (string-append (member-object obj indent) "?." name))
    (('index obj key)
     (string-append (member-object obj indent) "[" (expr key 0 indent) "]"))
    (('binop op a b)
     (let ((p (binop-precedence op)))
       (if (string=? op "**")
           (string-append (expr a 16 indent) " ** " (expr b p indent))
           ;; && and || are associative: a || (b || c) => a || b || c
           (let* ((right-prec (if (and (member op '("&&" "||"))
                                       (match b (('binop (? (cut string=? <> op)) . _) #t) (_ #f)))
                                  p
                                  (+ p 1)))
                  ;; ?? can't be mixed with && or || without parentheses.
                  (nullish-prec (lambda (operand prec)
                                  (match (list op operand)
                                    ((or ("??" ('binop (or "&&" "||") . _))
                                         ((or "&&" "||") ('binop "??" . _)))
                                     99)
                                    (_ prec)))))
             (string-append (expr a (nullish-prec a p) indent) " " op " "
                            (expr b (nullish-prec b right-prec) indent))))))
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

;; The source locations of statements (see print-js-with-locations).
(define current-locations (make-parameter #f))
(define current-markers (make-parameter #f))

(define marker-start (integer->char #xe000))
(define marker-end (integer->char #xe001))

;; A marker for the source location SRC, which is removed after printing.
(define (marker src)
  (match (current-markers)
    ((and markers (count . table))
     (hashv-set! table count src)
     (set-car! markers (+ count 1))
     (string-append (string marker-start) (number->string count) (string marker-end)))))

(define (statement s indent)
  (let ((text (statement* s indent))
        (src (and (current-locations) (hashq-ref (current-locations) s #f))))
    (if src
        (let ((pad (indent-string indent)))
          (string-append pad (marker src) (substring text (string-length pad))))
        text)))

(define (statement* s indent)
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
      (('return value)
       (string-append pad "return" (if value (string-append " " (expr value 0 indent)) "")
                      ";\n"))
      (('if _ _ _)
       (string-append pad (if-statement s indent) "\n"))
      (('while label body)
       (string-append pad
                      (if label (string-append label ": ") "")
                      "while (true) " (block body indent) "\n"))
      (((and keyword (or 'break 'continue)) label)
       (string-append pad (symbol->string keyword) (if label (string-append " " label) "")
                      ";\n"))
      (('throw e)
       (string-append pad "throw " (expr e 0 indent) ";\n"))
      (('try body name handler)
       (string-append pad "try " (block body indent)
                      " catch (" name ") " (block handler indent) "\n"))
      (('async stmt)
       (string-append pad "async " (string-trim-both (statement stmt indent)) "\n"))
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

;; Print STMTS like print-js and return the printed code and the source map
;; mappings ((GENERATED-LINE GENERATED-COLUMN SOURCE-LINE SOURCE-COLUMN) ...)
;; of the statements with a source location in LOCATIONS (a hash table from
;; statements to Tree-IL source locations).
(define (print-js-with-locations stmts locations)
  (let ((table (make-hash-table)))
    (parameterize ((current-locations locations)
                   (current-markers (cons 0 table)))
      (extract-markers (print-js stmts) table))))

;; Remove the markers from TEXT and return the text and the mappings.
;; Columns are in UTF-16 code units, like in JavaScript.
(define (extract-markers text table)
  (let ((out (open-output-string))
        (n (string-length text)))
    (let lp ((i 0) (line 0) (column 0) (mappings '()))
      (if (= i n)
          (values (get-output-string out) (reverse mappings))
          (let ((c (string-ref text i)))
            (cond
             ((char=? c marker-start)
              (let* ((end (string-index text marker-end i))
                     (src (hashv-ref table (string->number (substring text (+ i 1) end))))
                     (source-line (assq-ref src 'line))
                     (source-column (assq-ref src 'column)))
                (lp (+ end 1) line column
                    (if (and source-line source-column)
                        (cons (list line column source-line source-column) mappings)
                        mappings))))
             ((char=? c #\newline)
              (write-char c out)
              (lp (+ i 1) (+ line 1) 0 mappings))
             (else
              (write-char c out)
              (lp (+ i 1) line (+ column (if (> (char->integer c) #xffff) 2 1)) mappings))))))))
