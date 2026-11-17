# GuileScript

## What is GuileScript?

[![CI](https://github.com/aconchillo/guilescript/actions/workflows/guile3.0.yml/badge.svg)](https://github.com/aconchillo/guilescript/actions/workflows/guile3.0.yml)

GuileScript compiles [Guile](https://www.gnu.org/software/guile/) to readable, modern JavaScript.

You write Scheme (with all of Guile's macro system) and you get the kind of
JavaScript you would write by hand: `const`, `function`, arrow functions,
`while` loops and ES modules. There is no runtime library: GuileScript uses
JavaScript data and semantics, so the generated code works with any
JavaScript library, npm package or bundler.

```scheme
(js-import "canvas-confetti" #:default confetti)

(define-public (celebrate! times)
  (let loop ((i 0))
    (when (< i times)
      (confetti #:particle-count 100 #:spread (* 10 (+ i 1)))
      (loop (+ i 1)))))
```

```javascript
import confetti from "canvas-confetti";

export function celebrate(times) {
  let i = 0;
  while (true) {
    if (i < times) {
      confetti({ particleCount: 100, spread: 10 * (i + 1) });
      i = i + 1;
      continue;
    }
    return;
  }
}
```

If you need full Guile semantics in the browser (tail calls everywhere,
continuations, the numeric tower...) have a look at [Guile Hoot](https://spritely.institute/hoot/) which compiles
Guile to WebAssembly. GuileScript is the thin alternative: Scheme syntax and
macros on top of plain JavaScript.

## Why?

Because, why not? [Guile](https://www.gnu.org/software/guile/) is actually a lot of fun and I just got jealous that
people could do these sort of [cool things with ClojureScript](https://twitter.com/zaneshelby/status/1477988369154121734?s=20) and I couldn't with
Guile. But thanks to GuileScript it is now with [gas](https://github.com/aconchillo/gas).

## How does it work?

GuileScript leverages [Guile's Compiler Tower](https://www.gnu.org/software/guile/manual/html_node/Compiler-Tower.html). Guile expands all the macros
and compiles the code into [Tree-IL](https://www.gnu.org/software/guile/manual/html_node/Tree_002dIL.html). GuileScript then compiles Tree-IL into a
small JavaScript AST that is finally printed.

## How do I try it out?

You need Guile 3.0. Clone the repository and run:

```console
$ autoreconf -vif
$ ./configure
$ make
$ make check    # needs Node.js
$ sudo make install
```

If you are on macOS you can actually install GuileScript through [Guile Homebrew](https://github.com/aconchillo/homebrew-guile):

```console
$ brew install aconchillo/guile/guilescript
```

Then compile one of the examples:

```console
$ guilescript examples/basic/fibonacci.gs
$ guilescript -o fibonacci.js examples/basic/fibonacci.gs
$ node fibonacci.js
89
```

```scheme
(define (fib n)
  (if (<= n 1)
      1
      (+ (fib (- n 2)) (fib (- n 1)))))

(console-log (fib 10))
```

```javascript
function fib(n) {
  return n <= 1 ? 1 : fib(n - 2) + fib(n - 1);
}

console.log(fib(10));
```

GuileScript can also be used from a Guile program, e.g. a web server:
`compile-file` and `compile-modules` (from `(language guilescript compile)`)
compile GuileScript files into JavaScript (see
`examples/task-manager/server.scm`).

### Examples

- `examples/basic`: small programs, and a Guile web server with GuileScript
  code (`fibonacci-server.scm`).

- `examples/vite`: a minimal Vite project.

- `examples/task-manager`: a complete application. A Guile web server with a
  JSON API (tasks stored in SQLite), and the web page in GuileScript in two
  versions: using the DOM directly, and with React and Vite. The task
  validation is shared by the server and the web pages.

### Source maps

With `--source-map`, GuileScript also generates source maps (`fibonacci.js.map`
next to the compiled file, or inline when writing to stdout), so browsers'
developer tools show the `.gs` files and stack traces point to them. With
Node.js, use `node --enable-source-maps`:

```scheme
(define (fail-here)
  (error "boom"))

(fail-here)
```

```console
$ guilescript -o app.js --source-map app.gs
$ node --enable-source-maps app.js
/path/to/app.gs:2
  (error "boom"))
  ^

Error: boom
    at failHere (/path/to/app.gs:2:3)
    at Object.<anonymous> (/path/to/app.gs:4:1)
```

Statements are mapped to the line (and the approximate column) of the
GuileScript code they come from.

## Using npm packages and ES modules

`js-import` imports JavaScript modules and `define-public` (or `export`)
exports definitions. A file that uses them is an ES module that can be used
with any bundler (esbuild, Vite, webpack...).

| GuileScript | JavaScript |
|---|---|
| `(js-import "react" use-state)` | `import { useState } from "react";` |
| `(js-import "lodash" #:default _)` | `import _ from "lodash";` |
| `(js-import "node:path" #:as path)` | `import * as path from "node:path";` |
| `(js-import "./style.css")` | `import "./style.css";` |
| `(js-import "node:path" #:rename '((join . path-join)))` | `import { join as pathJoin } from "node:path";` |
| `(define-public (f x) ...)` | `export function f(x) { ... }` |
| `(export f g)` | adds `export` to the definitions of `f` and `g` |

## What works and what never will

Guile is two things: a language (its syntax and macros) and a runtime (its
data types, ports, continuations, threads...). GuileScript compiles the
language and uses JavaScript as the runtime. So, for any feature:

- If it only exists at compile time, it works: macros, `syntax-rules`,
  `syntax-case`, quasiquote...

- If JavaScript has an equivalent, it works or it will: numbers, strings,
  lists and vectors (arrays), hash tables (`Map`), closures, loops, errors,
  records (classes), multiple values (arrays), modules (ES modules)...

- If it only exists in Guile's runtime, it will never work: ports,
  first-class continuations (`call/cc`), `dynamic-wind`, threads, the numeric
  tower (exact rationals, automatic big integers), mutable pairs
  (`set-cdr!`), circular lists, `eval` at runtime... Use the JavaScript
  equivalents instead (`console`, `fetch`, streams, promises...).

If you need Guile's runtime in the browser, use [Guile Hoot](https://spritely.institute/hoot/).

## GuileScript modules

A GuileScript program can be split into modules. The module `(app util)` is
the file `app/util.gs`:

```scheme
(define-module (app util)
  #:export (shout))

(define (shout s)
  (string-append (string-upcase s) "!"))
```

```scheme
(define-module (app main)
  #:use-module (app util))

(console-log (shout "hello"))
```

Each module is compiled into an ES module, and importing a GuileScript module
imports the names it exports:

```javascript
import { shout } from "./util.js";
console.log(shout("hello"));
```

Compile a file and all the GuileScript modules it imports with `-d`:

```console
$ guilescript -d dist src/app/main.gs
```

This writes `dist/app/main.js` and `dist/app/util.js`. GuileScript modules are
searched relative to the module being compiled (`src/` here) and in the
directories given with `-L`. `#:select` and `#:prefix` work as usual.

GuileScript modules export functions and values. Macros can be shared through
Guile modules. A GuileScript module that only uses what works the same way
in Guile can also be loaded by Guile, e.g. to share code between a Guile
server and a web page (see `examples/task-manager`).

## Using with Vite

`vite-plugin-guilescript` (in this repository) lets [Vite](https://vite.dev/)
compile `.gs` files, so they can be used like any other module, both with the
development server and in production builds:

```js
// vite.config.js
import { defineConfig } from "vite";
import guilescript from "vite-plugin-guilescript";

export default defineConfig({
  plugins: [guilescript()],
});
```

```html
<script type="module" src="/src/main.gs"></script>
```

Compilation errors are shown with their location in Vite's error overlay, and
source maps map the code back to the `.gs` files (enable `build.sourcemap` to
also have them in production builds). The plugin is not published on npm
yet, so install it from this repository:

```console
$ npm install --save-dev /path/to/guilescript/vite-plugin-guilescript
```

See `examples/vite` for a complete example. (The plugin compiles each file
with `guilescript --source-imports`, which imports GuileScript modules by the
path of their `.gs` file, e.g. `./util.gs`, so they are compiled by the plugin
too.)

## Guile modules

Modules that are not GuileScript modules are Guile modules. For every name
imported from a Guile module:

- Macros (`match`, `cut`, `receive`...) can always be used, they are expanded
  at compile time. This also means macros can be shared between Guile code
  and GuileScript code.

- Procedures that GuileScript compiles inline (see [Built-in procedures](#built-in-procedures)) can be
  used, also when they are renamed with `#:select` or `#:prefix`.

- Any other procedure is a compilation error, since it would only exist in
  Guile.

```scheme
(use-modules (ice-9 match)
             ((srfi srfi-1) #:prefix s1:))

(define (area shape)
  (match shape
    (('circle r) (* 3 r r))
    (('rect w h) (* w h))))

(s1:fold + 0 (map area shapes))
```

Some common modules:

| Module | What can be used |
|---|---|
| `(ice-9 match)`, `(srfi srfi-26)`, `(srfi srfi-2)` | everything (they are macros) |
| `(srfi srfi-1)`, `(srfi srfi-13)` | the built-in procedures (`fold`, `iota`, ...) |
| `(srfi srfi-9)` | `define-record-type` (GuileScript's own) |
| `(ice-9 receive)`, `(srfi srfi-11)` | everything (multiple values are arrays) |
| `(ice-9 rdelim)`, `(ice-9 textual-ports)` | never (ports) |

There's no need to check this by hand: anything that can't be used is a
compilation error that says which name and which module.

## HTML elements

`(guilescript html)` (a Guile module shipped with GuileScript) has the `html`
macro, to write HTML elements with an SXML-like syntax instead of JSX:

```scheme
(use-modules (guilescript html))
(js-import "react" #:rename '((createElement . h)))

(define (<task-item> props)
  (let ((task (.-task props)))
    (html (li #:class-name ,(if (.-done task) "task done" "task")
              (input #:type "checkbox" #:checked ,(.-done task))
              (span #:class-name "title" ,(.-title task))))))

(define (<task-list> props)
  (html (ul ,(map (lambda (task) (html (<task-item> #:key ,(.-id task) #:task task)))
                  (.-tasks props)))))
```

`(tag #:property value ... child ...)` is an element: `tag` is an HTML tag,
or a component if it's like `<task-item>`. Property names are camelCase
(`#:class-name` is `className`). Values and children are strings, numbers,
variables, elements or `,expression` (any expression, e.g. a list of
elements). The code above compiles into:

```javascript
function TaskItem(props) {
  const task = props.task;
  return h(
    "li",
    { className: task.done ? "task done" : "task" },
    h("input", { type: "checkbox", checked: task.done }),
    h("span", { className: "title" }, task.title)
  );
}
```

`html` calls `h`, the [hyperscript](https://github.com/hyperhype/hyperscript)
convention `(h type properties child ...)`, so it works with any library with
such a function: React (`createElement`, imported as `h` above), Preact
(`(js-import "preact" h)`), or your own (see `examples/task-manager`, which
has both a React version and a version that creates DOM elements).

## Names

Scheme names are converted to camelCase JavaScript names, so the code reads
naturally from both sides.

| GuileScript | JavaScript |
|---|---|
| `log-counter` | `logCounter` |
| `even?` | `isEven` |
| `has-children?` | `hasChildren` |
| `set-name!` | `setName` |
| `list->vector` | `listToVector` |
| `*debug*` | `debug` |
| `new` | `new_` |
| `<http-request>` | `HttpRequest` |

It is a compilation error if two different Scheme names end up with the same
JavaScript name.

## Globals

Standard JavaScript globals (`Math`, `JSON`, `Promise`, `console`, `fetch`,
`setTimeout`...) can be used directly. Other globals, like the browser's
`document` or Node.js' `process`, have to be declared:

```scheme
(js-global document window)
```

Using a name that is not defined, imported or declared is a compilation
error, so typos are found at compile time instead of at runtime.

## Data and semantics

GuileScript uses JavaScript data types and JavaScript semantics.

| GuileScript | JavaScript |
|---|---|
| `#t`, `#f` | `true`, `false` |
| `#nil` | `null` |
| `42`, `0.5`, `1/2` | `42`, `0.5`, `0.5` |
| `"guile"`, `#\g` | `"guile"`, `"g"` |
| `'guile`, `#:guile` | `"guile"` |
| `'(1 2 3)`, `(list 1 2 x)` | `[1, 2, 3]`, `[1, 2, x]` |
| `#(1 2 3)`, `(vector 1 2 x)` | `[1, 2, 3]`, `[1, 2, x]` |
| `` `(1 ,x ,@xs) `` | `[1, x, ...xs]` |
| `'((name . "Ann") (age . 30))` | `{ name: "Ann", age: 30 }` |
| `(make-hash-table)` | `new Map()` |
| `(define-record-type <point> ...)` | `class Point { ... }` |
| `(values a b)`, `(values x)` | `[a, b]`, `x` |
| `(receive (a b) expr body ...)` | `const [a, b] = expr; body ...` |

A few things to keep in mind:

- Conditionals use JavaScript truthiness, so `0` and `""` are false.

- Lists and vectors are both arrays, so `cons` and `cdr` copy the array.

- Symbols are strings, so `eq?` on two symbols compares strings.

- `equal?` is JavaScript's `===`, it doesn't compare arrays deeply.

- Multiple values are arrays (a single value is just the value), so
  `receive`, `let-values`, `define-values` and `call-with-values` destructure
  arrays.

- Like in Guile, vector literals are constants: `#(1 2 x)` contains the
  symbol `x`. Use `(vector 1 2 x)` or `` `#(1 2 ,x) `` instead.

- A dotted pair that is not part of a quoted association list has no
  JavaScript equivalent and is a compilation error.

## Functions and control flow

| GuileScript | JavaScript |
|---|---|
| `(define a 23)` | `const a = 23;` |
| `(define (f x y) ...)` | `function f(x, y) { ... }` |
| `(lambda (x) (* x 2))` | `(x) => x * 2` |
| `(define (f a . rest) ...)` | `function f(a, ...rest) { ... }` |
| `(define* (f a #:optional (b 1)))` | `function f(a, b = 1) { ... }` |
| `(define* (f #:key (a 1) b))` | `function f({ a = 1, b } = {}) { ... }` |
| `(f 1 #:color "red" #:size 2)` | `f(1, { color: "red", size: 2 })` |
| `(let ((x 1)) ...)` | `const x = 1; ...` |
| `(if a b c)` | `a ? b : c`, or an `if` statement |
| `(and a b)`, `(or a b)` | `a ? b : false`, `a \|\| b` |
| `(error "Bad value:" x)` | `throw new Error(["Bad value:", x].join(" "))` |
| `(raise obj)` | `throw obj` |
| `(guard (e clause ...) body ...)` | `try { body } catch (e) { clause ... }` |

`guard` is the R7RS one: its clauses are `cond` clauses and, if none of them
matches, the exception is raised again.

```scheme
(define (parse-json s)
  (guard (e ((error-object? e) (console-error (error-object-message e)) #f))
    (.parse JSON s)))
```

`cond`, `case`, `when`, `unless`, `let*`, `letrec`, internal definitions,
`case-lambda`, `apply`, quasiquote and all macros (`define-syntax`,
`syntax-rules`, `syntax-case`) work as expected.

Named `let` loops, `do` loops and functions that call themselves in tail
position are compiled into `while` loops, so they don't grow the stack. Other
tail calls (e.g. mutually recursive functions) are regular JavaScript calls.

## Records

`define-record-type` (R7RS, SRFI-9) defines a JavaScript class, and calls to
the record procedures are compiled into JavaScript expressions:

```scheme
(define-record-type <point>
  (make-point x y)
  point?
  (x point-x set-point-x!)
  (y point-y))

(define p (make-point 1 2))
(set-point-x! p (+ (point-x p) 1))
```

```javascript
class Point {
  constructor(x, y) {
    this.x = x;
    this.y = y;
  }
}

const p = new Point(1, 2);
p.x = p.x + 1;
```

The record type (`<point>`) is the class (`Point`). The record procedures are
only defined as functions if they are exported or used as values (e.g.
`(map point-x points)`).

GuileScript's `define-record-type`, `guard` and `define-values` are always
used, even if a module providing Guile's versions (e.g. `(srfi srfi-9)`) is
imported.

## JavaScript interoperability

| GuileScript | JavaScript |
|---|---|
| `(.method obj arg1 ... argN)` | `obj.method(arg1, ..., argN)` |
| `(.to-fixed x 2)` | `x.toFixed(2)` |
| `(.-prop obj)` | `obj.prop` |
| `(set! (.-prop obj) value)` | `obj.prop = value` |
| `(js-ref obj 'prop)` | `obj.prop` |
| `(js-ref obj 'a 'b)` | `obj.a.b` |
| `(js-ref obj key)` | `obj[key]` |
| `(js-set! obj 'prop value)` | `obj.prop = value` |
| `(set! (js-ref obj 'a 'b) value)` | `obj.a.b = value` |
| `(js-invoke obj 'method arg1 ... argN)` | `obj.method(arg1, ..., argN)` |
| `(js-new Date arg1 ... argN)` | `new Date(arg1, ..., argN)` |
| `(js-obj #:name "Ann" 'age 30)` | `{ name: "Ann", age: 30 }` |
| `(js-typeof x)` | `typeof x` |
| `(js-instanceof x Date)` | `x instanceof Date` |
| `(js-delete! obj 'prop)` | `delete obj.prop` |
| `(js-await promise)` | `await promise` |

A function that uses `js-await` is compiled into an `async` function. At the
top level of an ES module, `js-await` is a top-level `await`.

```scheme
(define (fetch-json url)
  (.json (js-await (fetch url))))
```

```javascript
async function fetchJson(url) {
  return (await fetch(url)).json();
}
```

Methods and properties can also be used as values: `(map .toUpperCase
strings)` compiles to `strings.map((x) => x.toUpperCase())` and `(map .-length
strings)` to `strings.map((x) => x.length)`.

Method and property names (`.to-fixed`, `.-max-value`) and keywords
(`#:first-name`) are converted to camelCase like any other name (`toFixed`,
`maxValue`, `firstName`). Symbols and strings given to `js-ref`, `js-invoke`
and `js-obj` are used as they are.

## Built-in procedures

These procedures are compiled inline into JavaScript (see
`language/javascript/primitives.scm`):

- Numbers: `+ - * / = < > <= >= 1+ 1- abs ceiling floor round truncate max
  min sqrt exp log sin cos tan asin acos atan expt quotient remainder modulo
  zero? positive? negative? odd? even? number? integer? nan? exact inexact
  exact->inexact inexact->exact number->string string->number`

- Booleans and types: `not eq? eqv? equal? boolean? string? symbol? keyword?
  char? procedure? list? vector? null? pair?`

- Strings and characters: `string-length string-ref substring string-copy
  string-append string string-upcase string-downcase string-null?
  string-prefix? string-suffix? string-split string-join string-trim
  string-trim-right string-trim-both string->list list->string string=?
  string<? string>? string<=? string>=? char=? char<? char>? char<=? char>=?
  char->integer integer->char char-upcase char-downcase string->symbol
  symbol->string keyword->string`

- Lists and vectors: `list cons car cdr caar cadr cdar cddr caddr cdddr
  cadddr first second third fourth fifth last length list-ref list-tail
  list-head take drop append reverse list-copy map for-each filter remove
  fold fold-right reduce any every find count delete delete-duplicates
  member memq memv iota make-list sort vector make-vector vector-length
  vector-ref vector-set! vector-fill! vector-copy vector->list list->vector`

- Association lists and hash tables: `assoc-ref assq-ref assv-ref
  make-hash-table hash-ref hash-set! hash-remove! hash-clear! hash-for-each
  hash-map->list`

- Multiple values: `values call-with-values`

- Errors: `error-object? error-object-message`

- Logging: `console-log console-debug console-info console-warn
  console-error`

Using any other Guile procedure (e.g. `display`) is a compilation error (see
also [Guile modules](#guile-modules)).

## What's missing?

- `this` and classes (and `finally`).

- A small (tree-shakeable) runtime for things that can't be done inline, like
  a deep `equal?` or `format`.

- Continuations, `dynamic-wind` and parameters.


## Hacking

Run the test suite with `make check` (it needs Node.js). Each test in `tests/`
is a GuileScript program with its expected output in `;; => ...` comments
(or an expected compilation error in `;; error: ...` comments). Tests are
compiled with `-d` and `--source-map`, so they can import GuileScript modules
(e.g. the ones in `tests/project/`), and they are run with source maps
enabled.

## License

Copyright (C) 2022-2026 Aleix Conchillo Flaque <aconchillo@gmail.com>

GuileScript is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

GuileScript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GuileScript. If not, see https://www.gnu.org/licenses/.
