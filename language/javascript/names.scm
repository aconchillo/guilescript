;;; (language javascript names) --- GuileScript.

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

;; Mapping of Scheme identifiers to JavaScript identifiers.
;;
;;   foo-bar      -> fooBar
;;   even?        -> isEven       (is-empty? -> isEmpty)
;;   set-name!    -> setName
;;   list->vector -> listToVector
;;   *debug*      -> debug
;;   %internal    -> _internal
;;   new          -> new_         (JavaScript reserved words)
;;   <http-request> -> HttpRequest (types, e.g. record types)
;;   u:shout      -> uShout       (module prefixes)

;;; Code:

(define-module (language javascript names)
  #:use-module (srfi srfi-1)
  #:export (scheme->js-name
            keyword->js-name
            method-symbol?
            method-js-name
            property-symbol?
            property-js-name
            js-identifier-name?
            js-reserved?
            js-protected-global?
            js-standard-global?))

;; Words that can never be used as a variable name.
(define %reserved-words
  '("arguments" "await" "break" "case" "catch" "class" "const" "continue"
    "debugger" "default" "delete" "do" "else" "enum" "eval" "export"
    "extends" "false" "finally" "for" "function" "if" "implements" "import"
    "in" "instanceof" "interface" "let" "new" "null" "package" "private"
    "protected" "public" "return" "static" "super" "switch" "this" "throw"
    "true" "try" "typeof" "var" "void" "while" "with" "yield"))

;; Globals that can be referenced from GuileScript code (and that the compiler
;; itself might reference), so local variables must never shadow them.
(define %protected-globals
  '("undefined" "NaN" "Infinity" "globalThis" "console" "Math" "Number"
    "String" "Array" "Object" "JSON" "Error" "Map" "Set" "Symbol" "Promise"
    "parseInt"))

(define (make-string-table strings)
  (let ((table (make-hash-table)))
    (for-each (lambda (s) (hash-set! table s #t)) strings)
    table))

;; Globals that can be used without being declared with js-global: the
;; ECMAScript ones and the ones available both in browsers and Node.js.
(define %standard-globals
  '("globalThis" "Infinity" "NaN" "undefined" "isFinite" "isNaN" "parseFloat"
    "parseInt" "decodeURI" "decodeURIComponent" "encodeURI"
    "encodeURIComponent" "AggregateError" "Array" "ArrayBuffer" "Atomics"
    "BigInt" "BigInt64Array" "BigUint64Array" "Boolean" "DataView" "Date"
    "Error" "EvalError" "FinalizationRegistry" "Float32Array" "Float64Array"
    "Function" "Int8Array" "Int16Array" "Int32Array" "Intl" "JSON" "Map"
    "Math" "Number" "Object" "Promise" "Proxy" "RangeError" "ReferenceError"
    "Reflect" "RegExp" "Set" "SharedArrayBuffer" "String" "Symbol"
    "SyntaxError" "TypeError" "URIError" "Uint8Array" "Uint8ClampedArray"
    "Uint16Array" "Uint32Array" "WeakMap" "WeakRef" "WeakSet"
    ;; Browsers and Node.js
    "console" "setTimeout" "clearTimeout" "setInterval" "clearInterval"
    "queueMicrotask" "structuredClone" "fetch" "URL" "URLSearchParams"
    "TextEncoder" "TextDecoder" "AbortController" "AbortSignal" "Event"
    "EventTarget" "atob" "btoa" "crypto" "performance" "Blob" "FormData"
    "Headers" "Request" "Response"))

(define reserved-table (make-string-table %reserved-words))
(define protected-table (make-string-table %protected-globals))
(define standard-table (make-string-table %standard-globals))

(define (js-reserved? s)
  (hash-ref reserved-table s #f))

(define (js-protected-global? s)
  (hash-ref protected-table s #f))

(define (js-standard-global? s)
  (hash-ref standard-table s #f))

(define (identifier-start-char? c)
  (or (char-alphabetic? c) (char=? c #\_) (char=? c #\$)))

(define (identifier-char? c)
  (or (identifier-start-char? c) (char-numeric? c)))

;; Valid as a property name (obj.name), where reserved words are allowed.
(define (js-identifier-name? s)
  (and (positive? (string-length s))
       (identifier-start-char? (string-ref s 0))
       (string-every identifier-char? s)))

;; Characters that are valid in Scheme identifiers but not in JavaScript
;; ones. They become words in the camelCased result.
(define %char-words
  '((#\+ . "plus") (#\* . "star") (#\/ . "slash") (#\< . "lt") (#\> . "gt")
    (#\= . "eq") (#\! . "bang") (#\? . "p") (#\% . "pct") (#\& . "and")
    (#\~ . "tilde") (#\^ . "caret") (#\. . "dot")
    (#\@ . "at") (#\# . "hash")))

(define (string-replace-all s from to)
  (let ((n (string-length from)))
    (let lp ((start 0) (acc '()))
      (let ((i (string-contains s from start)))
        (if i
            (lp (+ i n) (cons* to (substring s start i) acc))
            (string-concatenate-reverse (cons (substring s start) acc)))))))

(define (strip-earmuffs s)
  (let ((n (string-length s)))
    (if (and (> n 2)
             (char=? (string-ref s 0) #\*)
             (char=? (string-ref s (- n 1)) #\*))
        (substring s 1 (- n 1))
        s)))

(define (predicate->word s)
  (let ((n (string-length s)))
    (if (and (> n 1) (char=? (string-ref s (- n 1)) #\?))
        (let ((base (substring s 0 (- n 1))))
          (if (or (string-prefix? "is-" base) (string-prefix? "has-" base))
              base
              (string-append "is-" base)))
        s)))

(define (strip-bang s)
  (let ((n (string-length s)))
    (if (and (> n 1) (char=? (string-ref s (- n 1)) #\!))
        (substring s 0 (- n 1))
        s)))

(define (capitalize s)
  (if (string-null? s)
      s
      (string-append (string (char-upcase (string-ref s 0))) (substring s 1))))

(define (words->camel-case words)
  (if (null? words)
      ""
      (string-concatenate (cons (car words) (map capitalize (cdr words))))))

(define (spell-out-invalid-chars s)
  (string-concatenate
   (map (lambda (c)
          (cond
           ((or (identifier-char? c) (char=? c #\-)) (string c))
           ;; Prefixes (e.g. u:shout) separate words too.
           ((char=? c #\:) "-")
           ((assv-ref %char-words c) => (lambda (w) (string-append "-" w "-")))
           (else (string-append "-u" (number->string (char->integer c) 16) "-"))))
        (string->list s))))

;; Convert a Scheme identifier (symbol or string) into a JavaScript
;; identifier. With #:property? #t the result is used as a property name, so
;; reserved words are left untouched.
(define (type-name? s)
  (and (> (string-length s) 2)
       (string-prefix? "<" s)
       (string-suffix? ">" s)))

(define* (scheme->js-name name #:key (property? #f))
  (let ((s (if (symbol? name) (symbol->string name) name)))
    (if (type-name? s)
        (capitalize (scheme->js-name (substring s 1 (- (string-length s) 1))
                                     #:property? #t))
        (identifier->js-name s property?))))

(define (identifier->js-name s property?)
  (let* ((s s)
         (internal? (and (> (string-length s) 1)
                         (char=? (string-ref s 0) #\%)))
         (s (if internal? (substring s 1) s))
         (s (strip-earmuffs s))
         (s (predicate->word s))
         (s (strip-bang s))
         (s (string-replace-all s "->" "-to-"))
         (s (spell-out-invalid-chars s))
         (words (remove string-null? (string-split s #\-)))
         (s (words->camel-case words))
         (s (cond
             ((string-null? s) "_")
             (internal? (string-append "_" s))
             ((char-numeric? (string-ref s 0)) (string-append "_" s))
             (else s))))
    (if (and (not property?) (js-reserved? s))
        (string-append s "_")
        s)))

(define (keyword->js-name kw)
  (scheme->js-name (keyword->symbol kw) #:property? #t))

;; Method call syntax: (.method obj arg ...) => obj.method(arg, ...). The
;; method name follows the same rules as other names, so (.to-fixed x 2)
;; => x.toFixed(2). Note that ... (the ellipsis) is not a method.
(define (method-symbol? sym)
  (and (symbol? sym)
       (let ((s (symbol->string sym)))
         (and (> (string-length s) 1)
              (char=? (string-ref s 0) #\.)
              (not (memv (string-ref s 1) '(#\. #\-)))))))

(define (method-js-name sym)
  (scheme->js-name (substring (symbol->string sym) 1) #:property? #t))

;; Property access syntax: (.-prop obj) => obj.prop.
(define (property-symbol? sym)
  (and (symbol? sym)
       (let ((s (symbol->string sym)))
         (and (> (string-length s) 2)
              (string-prefix? ".-" s)))))

(define (property-js-name sym)
  (scheme->js-name (substring (symbol->string sym) 2) #:property? #t))
