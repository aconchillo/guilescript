# GuileScript

[![CI](https://github.com/aconchillo/guilescript/actions/workflows/guile3.0.yml/badge.svg)](https://github.com/aconchillo/guilescript/actions/workflows/guile3.0.yml)

GuileScript compiles Scheme code into JavaScript. Scheme is a small language
from the Lisp family, and GuileScript uses the version of it implemented by
[Guile](https://www.gnu.org/software/guile/), the GNU project's Scheme.

The JavaScript you get is plain and readable, like code you would write by
hand. It runs in the browser and in Node.js, and it can use any JavaScript
library. For example, this is a React component written in GuileScript:

```scheme
(use-modules (guilescript html))
(js-import "react" #:rename '((createElement . h)) use-state)

(define-public (<counter>)
  (define-values (count set-count!) (use-state 0))
  (html (button #:on-click ,(lambda () (set-count! (+ count 1)))
                "Clicked " ,count " times")))
```

and the JavaScript it compiles into:

```javascript
import { createElement as h, useState } from "react";

export function Counter() {
  const [count, setCount] = useState(0);
  return h(
    "button",
    { onClick: () => setCount(count + 1) },
    "Clicked ",
    count,
    " times"
  );
}
```

There's no JSX: the HTML elements are written in Scheme too.

## Why GuileScript?

- **Readable output.** The generated code uses `const`, functions, arrow
  functions and ES modules, so you can read it and debug it. Source maps
  point errors back to your `.gs` files.

- **No runtime library.** GuileScript uses JavaScript's own data (a Scheme
  list is a JavaScript array, a string is a string), so the code works with
  any npm package and bundler, and JavaScript code can call it.

- **Macros.** Scheme macros let you add new syntax to the language. The
  `html` syntax above is a macro of about 40 lines, and you can write your
  own.

- **Mistakes caught early.** Using a name that doesn't exist, or a Guile
  feature that can't work in JavaScript, is a compilation error instead of a
  surprise at runtime.

- **The same language on the server and in the browser.** Code can be shared
  between a Guile program and its web page (see the
  [task manager](examples/task-manager) example).

If you need all of Guile in the browser (continuations, exact numbers...),
have a look at [Guile Hoot](https://spritely.institute/hoot/), which compiles
Guile to WebAssembly. GuileScript is the lightweight alternative: Scheme's
syntax and macros on top of plain JavaScript.

## Getting started

### Install

GuileScript needs [Guile 3.0](https://www.gnu.org/software/guile/). On macOS,
you can install both with [Guile Homebrew](https://github.com/aconchillo/homebrew-guile):

```console
$ brew install aconchillo/guile/guilescript
```

Otherwise, install Guile 3.0 and its development files (e.g. `guile-3.0-dev`
on Debian and Ubuntu), pkg-config, Autoconf and Automake with your system's
package manager, and build GuileScript:

```console
$ git clone https://github.com/aconchillo/guilescript.git
$ cd guilescript
$ autoreconf -vif
$ ./configure
$ make
$ sudo make install
```

### Your first program

GuileScript files end in `.gs`. Write this into `hello.gs`:

```scheme
(define (greet name)
  (string-append "Hello, " name "!"))

(console-log (greet "world"))
```

`guilescript` prints the JavaScript code, or writes it to a file with `-o`:

```console
$ guilescript hello.gs
function greet(name) {
  return "Hello, " + name + "!";
}

console.log(greet("world"));
$ guilescript -o hello.js hello.gs
$ node hello.js
Hello, world!
```

### A React app with Vite

[Vite](https://vite.dev/) is a popular tool to develop and build web
applications. Its GuileScript plugin, `vite-plugin-guilescript`, compiles
`.gs` files when they are imported. To create a React app from scratch:

```console
$ mkdir hello-react
$ cd hello-react
$ npm init -y
$ npm install react react-dom
$ npm install --save-dev vite vite-plugin-guilescript
```

Then add these three files. `vite.config.js` enables the plugin:

```js
import { defineConfig } from "vite";
import guilescript from "vite-plugin-guilescript";

export default defineConfig({
  plugins: [guilescript()],
});
```

`index.html` loads `src/main.gs`:

```html
<!doctype html>
<html>
  <body>
    <div id="root"></div>
    <script type="module" src="/src/main.gs"></script>
  </body>
</html>
```

And `src/main.gs` is the counter from above, shown in the page:

```scheme
(use-modules (guilescript html))
(js-import "react" #:rename '((createElement . h)) use-state)
(js-import "react-dom/client" create-root)
(js-global document)

(define (<counter>)
  (define-values (count set-count!) (use-state 0))
  (html (button #:on-click ,(lambda () (set-count! (+ count 1)))
                "Clicked " ,count " times")))

(.render (create-root (.get-element-by-id document "root"))
         (html (<counter>)))
```

Run `npx vite` and open the address it prints (http://localhost:5173). When
you change `src/main.gs`, the page reloads, and compilation errors are shown
in the page with the line they come from. `npx vite build` builds the app
into `dist/`.

## Scheme for JavaScript developers

Scheme code is made of lists in parentheses. The first element is the
function and the rest are its arguments, so `f(a, b)` is `(f a b)`. There
are no operators: `+` and `<` are functions too, so `1 + 2 * 3` is
`(+ 1 (* 2 3))`.

| JavaScript                                  | GuileScript                         |
|---------------------------------------------|-------------------------------------|
| `f(a, b)`                                   | `(f a b)`                           |
| `1 + 2 * 3`                                 | `(+ 1 (* 2 3))`                     |
| `const x = 42;`                             | `(define x 42)`                     |
| `function add(a, b) { return a + b; }`      | `(define (add a b) (+ a b))`        |
| `(x) => x * 2`                              | `(lambda (x) (* x 2))`              |
| `x > 0 ? "yes" : "no"`                      | `(if (> x 0) "yes" "no")`           |
| `if (a) {...} else if (b) {...} else {...}` | `(cond (a ...) (b ...) (else ...))` |
| `const a = 1; const b = 2; ...`             | `(let ((a 1) (b 2)) ...)`           |
| `true`, `false`, `null`                     | `#t`, `#f`, `#nil`                  |
| `[1, 2, 3]`                                 | `(list 1 2 3)`                      |
| `{ name: "Ann", age: 30 }`                  | `(js-obj #:name "Ann" #:age 30)`    |
| `user.name`                                 | `(.-name user)`                     |
| `user.name = "Bob"`                         | `(set! (.-name user) "Bob")`        |
| `s.toUpperCase()`                           | `(.to-upper-case s)`                |
| `items.map((x) => x * 2)`                   | `(map (lambda (x) (* x 2)) items)`  |
| `new Date()`                                | `(js-new Date)`                     |
| `await fetch(url)`                          | `(js-await (fetch url))`            |
| `try { ... } catch (e) { ... }`             | `(guard (e (#t ...)) ...)`          |
| `import { useState } from "react";`         | `(js-import "react" use-state)`     |
| `export function add(a, b) { ... }`         | `(define-public (add a b) ...)`     |
| `console.log("hi")`                         | `(console-log "hi")`                |

A few more things that will help you read Scheme code:

- Names can have dashes and other characters, and GuileScript converts them
  to camelCase: `user-name` is `userName`. By convention, the names of
  functions that return true or false end in `?`, and those that change
  something end in `!`: `valid?` is `isValid` and `set-name!` is `setName`.

- `#:name` is a keyword. In a call, keywords are named arguments, which are
  passed as an object: `(f 1 #:color "red")` is `f(1, { color: "red" })`.

- A function returns the value of its last expression, there's no `return`.

- Comments start with `;`.

- Only `#f`, `#nil` and `undefined` are false. Unlike in JavaScript, `0` and
  `""` are true.

To learn more about Scheme, start with [Guile's introduction to
Scheme](https://www.gnu.org/software/guile/manual/html_node/Hello-Scheme_0021.html).

## Writing React components

This is a to-do list (in `src/todos.gs`, if you want to try it in the app
above):

```scheme
(define-module (todos)
  #:use-module (guilescript html)
  #:export (<todo-list>))

(js-import "react" #:rename '((createElement . h)) use-state)

(define (<todo-list>)
  (define-values (todos set-todos!) (use-state '()))
  (define-values (text set-text!) (use-state ""))

  (define (add! event)
    (.prevent-default event)
    (unless (string-null? text)
      (set-todos! (append todos (list text)))
      (set-text! "")))

  (html
   (div
    (form #:on-submit add!
          (input #:value text
                 #:placeholder "What needs to be done?"
                 #:on-change ,(lambda (event) (set-text! (.-value (.-target event)))))
          (button "Add"))
    ,(if (null? todos)
         (html (p "Nothing to do!"))
         (html (ul ,(map (lambda (todo) (html (li #:key todo todo)))
                         todos)))))))
```

To use it, `src/main.gs` imports the module and shows `(<todo-list>)`:

```scheme
(use-modules (guilescript html) (todos))
(js-import "react" #:rename '((createElement . h)))
(js-import "react-dom/client" create-root)
(js-global document)

(.render (create-root (.get-element-by-id document "root"))
         (html (<todo-list>)))
```

How it works:

- `(html ...)` is an element: `(tag #:property value ... child ...)`.
  Properties are keywords, converted to camelCase (`#:on-change` is
  `onChange`, `#:class-name` is `className`). Children are other elements,
  strings, numbers or variables.

- A comma inserts any Scheme expression: a value (`,count`), a function
  (`,(lambda (event) ...)`), a list of elements (`,(map ...)`) or an
  element chosen with `if`.

- Components are functions named like `<todo-list>` (`TodoList` in
  JavaScript), used like tags: `(<todo-list>)`. They receive the
  properties as an object: `(.-title props)`.

- Hooks are imported from React like any other function. `define-values`
  takes the two values that `use-state` returns, like `const [todos,
  setTodos] = useState([])`.

- `(guilescript html)` is a Guile module that comes with GuileScript. The
  `html` macro calls `h(type, properties, ...children)`, which is why React's
  `createElement` is imported as `h`. It works the same with Preact or with
  your own `h` function (see the [task manager](examples/task-manager), which
  has a React version and one that creates DOM elements directly).

## What works and what doesn't

GuileScript compiles the Scheme language (its syntax and Guile's macros) and
uses JavaScript for everything else (the data, the libraries, the event
loop). So, in short: if JavaScript has it, it works.

**Works:**

- Functions (with optional, rest and keyword arguments), closures, `let`,
  `cond`, `case`, `when`, `unless`, loops (`do` and named `let`, compiled into
  `while` loops), quasiquote, `define-values`.
- All macros (`define-syntax`, `syntax-rules`, `syntax-case`), including
  macros from Guile modules like `match`.
- Numbers, strings, characters, symbols, lists and vectors (arrays), hash
  tables (`Map`), records (`define-record-type`, which defines a class),
  multiple values and errors (`error`, `raise`, `guard`).
- More than 150 built-in procedures (lists, strings, numbers...), compiled
  inline.
- JavaScript: npm packages, ES modules, methods, properties, `new`,
  `async`/`await` and globals.
- Modules: each GuileScript module is an ES module.

**Will never work**, because it needs Guile's runtime: continuations
(`call/cc`), `dynamic-wind`, ports (`display`, `read`...), threads, exact
fractions and big integers, changing pairs (`set-car!`), `eval`. Use the
JavaScript equivalents (`console-log`, `fetch`, promises...).

**Not yet:** `format`, `this` and defining JavaScript classes, `finally`, and
parameters.

You don't need to remember this: anything that can't be compiled is a
compilation error that says why. Since the data is JavaScript's, a few
things behave differently than in Guile (lists are arrays, symbols are
strings...). They are listed in the
[reference](doc/reference.md#data-and-semantics).

## Building on GuileScript

**Write your own macros.** Macros run when the code is compiled, so they cost
nothing at runtime. Put them in a Guile module, e.g. `macros/my/macros.scm`:

```scheme
(define-module (my macros)
  #:export (->))

;; (-> x (f a) g) is (g (f x a)): each result is the first argument of the
;; next call.
(define-syntax ->
  (syntax-rules ()
    ((_ x) x)
    ((_ x (f args ...) rest ...) (-> (f x args ...) rest ...))
    ((_ x f rest ...) (-> (f x) rest ...))))
```

and use them from GuileScript:

```scheme
(use-modules (my macros))

(define (shout s)
  (-> s string-trim-both string-upcase (string-append "!")))
```

```javascript
function shout(s) {
  return s.trim().toUpperCase() + "!";
}
```

The directory with the module is given with `-L macros` (or the plugin's
`loadPath` option in Vite). This is how `(guilescript html)` works too.

**Write libraries.** GuileScript modules are compiled into ES modules, so a
GuileScript library can be published on npm and used from JavaScript or
TypeScript, and the other way around.

**Use it from Guile.** `(language guilescript compile)` compiles GuileScript
from a Guile program, e.g. a web server that serves its own web page (see the
[task manager](examples/task-manager)).

**Integrate it with other tools.** The Vite plugin is about 150 lines of
JavaScript that run the `guilescript` command. Plugins for other tools can do
the same.

Everything is described in the [reference](doc/reference.md).

## Examples

- [`examples/basic`](examples/basic): small programs, and a Guile web server
  with GuileScript code (`fibonacci-server.scm`, and the same server written in
  [wisp](https://www.draketo.de/software/wisp) in `fibonacci-server.w`).

- [`examples/vite`](examples/vite): a minimal Vite project.

- [`examples/task-manager`](examples/task-manager): a complete application. A
  Guile web server with a JSON API (the tasks are stored in SQLite), and its
  web page in GuileScript, in two versions: using the DOM directly, and with
  React and Vite. The validation of the tasks is shared by the server and the
  web pages.

## How it works

GuileScript is built on [Guile's compiler tower](https://www.gnu.org/software/guile/manual/html_node/Compiler-Tower.html). Guile reads the code,
expands the macros and compiles it into
[Tree-IL](https://www.gnu.org/software/guile/manual/html_node/Tree_002dIL.html), Guile's
intermediate language. GuileScript then compiles Tree-IL into a small
JavaScript syntax tree, which is printed as JavaScript.

## History

I started GuileScript in 2022 because Guile is a lot of fun, but it stayed a
prototype: I never found the time to take it further, and I thought I might
come back to it when I retire. In 2026 I rewrote it with the help of a coding
agent (Claude Code), which made it possible to do it now instead.

## Hacking

To use GuileScript without installing it, run commands with the `env` script
of the build directory (created by `./configure`). It sets up Guile's load
paths and adds the `guilescript` command to `PATH`:

```console
$ ./env guilescript examples/basic/fibonacci.gs
```

Run the tests with `make check` (it needs Node.js). Each test in `tests/` is
a GuileScript program with its expected output in `;; => ...` comments (or
an expected compilation error in `;; error: ...` comments). Tests are
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
