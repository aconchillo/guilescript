# GuileScript reference

This is the complete description of GuileScript. For an introduction, see
the [README](../README.md).

- [Command line](#command-line)
- [Source maps](#source-maps)
- [npm packages and ES modules](#npm-packages-and-es-modules)
- [GuileScript modules](#guilescript-modules)
- [Guile modules](#guile-modules)
- [Vite](#vite)
- [Using GuileScript from Guile](#using-guilescript-from-guile)
- [Names](#names)
- [Globals](#globals)
- [Data and semantics](#data-and-semantics)
- [Functions and control flow](#functions-and-control-flow)
- [Records](#records)
- [JavaScript interoperability](#javascript-interoperability)
- [HTML elements](#html-elements)
- [Built-in procedures](#built-in-procedures)

## Command line

```console
$ guilescript [OPTION]... FILE...
```

- `-o FILE`, `--output=FILE`: write the JavaScript code to `FILE` (by
  default it's printed).

- `-d DIR`, `--output-dir=DIR`: compile the files and all the GuileScript
  modules they import into `DIR` (e.g. the module `(app util)` into
  `DIR/app/util.js`).

- `-L DIR`, `--load-path=DIR`: also search GuileScript modules and Guile
  modules (e.g. with macros) in `DIR`. It can be given more than once.

- `--source-map`: generate [source maps](#source-maps).

- `--source-imports`: import GuileScript modules by the path of their `.gs`
  file (e.g. `./util.gs`) instead of the compiled `.js` file, for bundler
  plugins.

## Source maps

With `--source-map`, GuileScript also generates source maps (`app.js.map`
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

## npm packages and ES modules

`js-import` imports JavaScript modules and `define-public` (or `export`)
exports definitions. A file that uses them is an ES module that can be used
with any bundler (esbuild, Vite, webpack...).

| GuileScript                                              | JavaScript                                      |
|----------------------------------------------------------|-------------------------------------------------|
| `(js-import "react" use-state)`                          | `import { useState } from "react";`             |
| `(js-import "lodash" #:default _)`                       | `import _ from "lodash";`                       |
| `(js-import "node:path" #:as path)`                      | `import * as path from "node:path";`            |
| `(js-import "./style.css")`                              | `import "./style.css";`                         |
| `(js-import "node:path" #:rename '((join . path-join)))` | `import { join as pathJoin } from "node:path";` |
| `(define-public (f x) ...)`                              | `export function f(x) { ... }`                  |
| `(export f g)`                                           | adds `export` to the definitions of `f` and `g` |

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
server and a web page (see [`examples/task-manager`](../examples/task-manager)).

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

| Module                                             | What can be used                              |
|----------------------------------------------------|-----------------------------------------------|
| `(ice-9 match)`, `(srfi srfi-26)`, `(srfi srfi-2)` | everything (they are macros)                  |
| `(srfi srfi-1)`, `(srfi srfi-13)`                  | the built-in procedures (`fold`, `iota`, ...) |
| `(srfi srfi-9)`                                    | `define-record-type` (GuileScript's own)      |
| `(ice-9 receive)`, `(srfi srfi-11)`                | everything (multiple values are arrays)       |
| `(ice-9 rdelim)`, `(ice-9 textual-ports)`          | never (ports)                                 |

There's no need to check this by hand: anything that can't be used is a
compilation error that says which name and which module.

## Vite

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
also have them in production builds). Install it with:

```console
$ npm install --save-dev vite-plugin-guilescript
```

It runs the `guilescript` command, so GuileScript must be installed (or see
[Hacking](../README.md#hacking) to use it without installing it).

Options:

- `command`: the `guilescript` command (default: the `GUILESCRIPT`
  environment variable, or `guilescript`).

- `loadPath`: directories (relative to the project root) where GuileScript
  modules and Guile modules (e.g. with macros) are also searched, like
  `guilescript -L`.

```js
guilescript({ loadPath: ["../shared"] })
```

See [`examples/vite`](../examples/vite) for a minimal project, and
[`examples/task-manager/react`](../examples/task-manager/react) for a
complete one. (The plugin compiles each file with `guilescript
--source-imports`, which imports GuileScript modules by the path of their
`.gs` file, e.g. `./util.gs`, so they are compiled by the plugin too.)

## Using GuileScript from Guile

`(language guilescript compile)` compiles GuileScript from a Guile program,
e.g. a web server that serves its own web page:

- `(compile exp)` compiles the expression `exp` (e.g. a quoted `begin` with
  definitions). It returns two values: the JavaScript code and the module
  where the code was expanded.

- `(compile-file file #:load-path dirs)` returns the JavaScript code of
  `file`.

- `(compile-modules files #:load-path dirs #:source-map? #t)` compiles
  `files` and all the GuileScript modules they import. It returns a list of
  `(path js source-map)`, where `path` is the path of the compiled module
  without extension (e.g. `"app/util"`) and `source-map` is the source map
  (as JSON), or `#f`.

```scheme
(use-modules (language guilescript compile))

(compile-modules '("web/app.gs") #:load-path '("shared"))
```

See [`examples/task-manager/server.scm`](../examples/task-manager/server.scm)
and [`examples/basic/fibonacci-server.scm`](../examples/basic/fibonacci-server.scm).

## Names

Scheme names are converted to camelCase JavaScript names, so the code reads
naturally from both sides.

| GuileScript      | JavaScript     |
|------------------|----------------|
| `log-counter`    | `logCounter`   |
| `even?`          | `isEven`       |
| `has-children?`  | `hasChildren`  |
| `set-name!`      | `setName`      |
| `list->vector`   | `listToVector` |
| `*debug*`        | `debug`        |
| `new`            | `new_`         |
| `<http-request>` | `HttpRequest`  |

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

GuileScript uses JavaScript data types, so values are passed to and from
JavaScript as they are.

| GuileScript                        | JavaScript                      |
|------------------------------------|---------------------------------|
| `#t`, `#f`                         | `true`, `false`                 |
| `#nil`                             | `null`                          |
| `42`, `0.5`, `1/2`                 | `42`, `0.5`, `0.5`              |
| `"guile"`, `#\g`                   | `"guile"`, `"g"`                |
| `'guile`, `#:guile`                | `"guile"`                       |
| `'(1 2 3)`, `(list 1 2 x)`         | `[1, 2, 3]`, `[1, 2, x]`        |
| `#(1 2 3)`, `(vector 1 2 x)`       | `[1, 2, 3]`, `[1, 2, x]`        |
| `` `(1 ,x ,@xs) ``                 | `[1, x, ...xs]`                 |
| `(js-obj #:name "Ann" #:age 30)`   | `{ name: "Ann", age: 30 }`      |
| `(make-hash-table)`                | `new Map()`                     |
| `(define-record-type <point> ...)` | `class Point { ... }`           |
| `(values a b)`, `(values x)`       | `[a, b]`, `x`                   |
| `(receive (a b) expr body ...)`    | `const [a, b] = expr; body ...` |

A few things to keep in mind:

- Like in Scheme, only `#f`, `#nil` and `undefined` are false: `0` and `""`
  are true (use `null?`, `string-null?` or `zero?` to check for them).
  `undefined` is false so that "not found" (`find`, `hash-ref`, a missing
  property) works like `#f`. When the compiler can't tell that a test is a
  boolean, it checks `x != null && x !== false`. Comparisons, predicates
  (procedures and variables whose names end in `?`) and JavaScript methods
  like `isArray` or `includes` are booleans.

- Lists and vectors are both arrays, so `cons` and `cdr` copy the array.

- Symbols are strings, so `eq?` on two symbols compares strings (and
  `string?` is true for symbols).

- `member`, `memq` and `memv` return `#t` instead of the rest of the list,
  and `any` and `every` return `#t` instead of the predicate's value. Both
  work the same in conditions.

- Like in Scheme, `equal?` compares lists and vectors element by element
  (and so do `member`, `delete` and `delete-duplicates`), while `eq?` and
  `eqv?` are `===`. A small `isEqual` function is added to the modules that
  need it. Hash tables are `Map`s, which compare keys with `===`, so lists
  used as keys are compared by identity.

- Multiple values are arrays (a single value is just the value), so
  `receive`, `let-values`, `define-values` and `call-with-values` destructure
  arrays.

- Like in Guile, vector literals are constants: `#(1 2 x)` contains the
  symbol `x`. Use `(vector 1 2 x)` or `` `#(1 2 ,x) `` instead.

- JavaScript has no pairs, so quoted data is always made of arrays, and
  dotted pairs (e.g. a quoted association list) are a compilation error.
  Use a list of lists to have something to iterate, or `js-obj` for an
  object:

  ```scheme
  (define filters '(("all" "All") ("done" "Done")))   ; [["all", "All"], ...]
  (map car filters)                                    ; ["all", "done"]

  (define labels (js-obj "all" "All" "done" "Done"))  ; { all: "All", ... }
  (assoc-ref labels "all")                             ; labels.all
  ```

  `assoc-ref` (and `assq-ref`, `assv-ref`) reads the properties of an
  object, so code that reads objects with it can be shared with Guile code,
  where they would be association lists.

## Functions and control flow

| GuileScript                        | JavaScript                                     |
|------------------------------------|------------------------------------------------|
| `(define a 23)`                    | `const a = 23;`                                |
| `(define (f x y) ...)`             | `function f(x, y) { ... }`                     |
| `(lambda (x) (* x 2))`             | `(x) => x * 2`                                 |
| `(define (f a . rest) ...)`        | `function f(a, ...rest) { ... }`               |
| `(define* (f a #:optional (b 1)))` | `function f(a, b = 1) { ... }`                 |
| `(define* (f #:key (a 1) b))`      | `function f({ a = 1, b } = {}) { ... }`        |
| `(f 1 #:color "red" #:size 2)`     | `f(1, { color: "red", size: 2 })`              |
| `(let ((x 1)) ...)`                | `const x = 1; ...`                             |
| `(if a b c)`                       | `a ? b : c`, or an `if` statement              |
| `(and a b)`, `(or a b)`            | `a ? b : false`, `a \|\| b`                    |
| `(error "Bad value:" x)`           | `throw new Error(["Bad value:", x].join(" "))` |
| `(raise obj)`                      | `throw obj`                                    |
| `(guard (e clause ...) body ...)`  | `try { body } catch (e) { clause ... }`        |

Optional and keyword arguments without a default value are `undefined` when
they are not given (in Guile they are `#f`). Both are false in conditions.

`guard` is the R7RS one: its clauses are `cond` clauses and, if none of them
matches, the exception is raised again.

```scheme
(define (parse-json s)
  (guard (e ((error-object? e) (console-error (error-object-message e)) #f))
    (.parse JSON s)))
```

`error-object-message` is the error's `message`, which includes the
irritants (`"Bad value: 42"`).

`cond`, `case`, `when`, `unless`, `let*`, `letrec`, internal definitions,
`case-lambda`, `apply`, quasiquote and all macros (`define-syntax`,
`syntax-rules`, `syntax-case`) work as expected. `case` compares with `===`,
so strings can be used as keys too.

Named `let` loops, `do` loops and functions that call themselves in tail
position are compiled into `while` loops, so they don't grow the stack. Other
tail calls (e.g. mutually recursive functions) are regular JavaScript calls,
so very deep recursion can run out of stack.

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

| GuileScript                             | JavaScript                    |
|-----------------------------------------|-------------------------------|
| `(.method obj arg1 ... argN)`           | `obj.method(arg1, ..., argN)` |
| `(.to-fixed x 2)`                       | `x.toFixed(2)`                |
| `(.-prop obj)`                          | `obj.prop`                    |
| `(set! (.-prop obj) value)`             | `obj.prop = value`            |
| `(js-ref obj 'prop)`                    | `obj.prop`                    |
| `(js-ref obj 'a 'b)`                    | `obj.a.b`                     |
| `(js-ref obj key)`                      | `obj[key]`                    |
| `(js-set! obj 'prop value)`             | `obj.prop = value`            |
| `(set! (js-ref obj 'a 'b) value)`       | `obj.a.b = value`             |
| `(js-invoke obj 'method arg1 ... argN)` | `obj.method(arg1, ..., argN)` |
| `(js-new Date arg1 ... argN)`           | `new Date(arg1, ..., argN)`   |
| `(js-obj #:name "Ann" 'age 30)`         | `{ name: "Ann", age: 30 }`    |
| `(js-typeof x)`                         | `typeof x`                    |
| `(js-instanceof x Date)`                | `x instanceof Date`           |
| `(js-delete! obj 'prop)`                | `delete obj.prop`             |
| `(js-await promise)`                    | `await promise`               |

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
elements). `(<> child ...)` is a fragment, like in JSX: it's the same as
`(<fragment> child ...)`, so `<fragment>` must be defined (e.g. imported
from React with `(js-import "react" <fragment>)`). The code above compiles
into:

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
(`(js-import "preact" h)`), or your own (see [`examples/task-manager`](../examples/task-manager), which
has both a React version and a version that creates DOM elements).

## Built-in procedures

These procedures are compiled inline into JavaScript (see
[`language/javascript/primitives.scm`](../language/javascript/primitives.scm)):

- Numbers: `+ - * / = < > <= >= 1+ 1- abs ceiling floor round truncate max
  min sqrt exp log sin cos tan asin acos atan expt quotient remainder modulo
  zero? positive? negative? odd? even? number? integer? nan? exact inexact
  exact->inexact inexact->exact number->string string->number floor/
  floor-quotient floor-remainder truncate/ truncate-quotient
  truncate-remainder`

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

- Objects and hash tables: `assoc-ref assq-ref assv-ref` (object
  properties), `make-hash-table hash-ref hash-set! hash-remove! hash-clear! hash-for-each
  hash-map->list`

- Multiple values: `values call-with-values`

- Errors: `error-object? error-object-message`

- Logging: `console-log console-debug console-info console-warn
  console-error`

Using any other Guile procedure (e.g. `display`) is a compilation error (see
also [Guile modules](#guile-modules)).
