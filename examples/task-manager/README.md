# Task manager

A small task manager written in Guile and GuileScript, in two versions: one
without any framework or bundler, and one with React and Vite. Both use the
same server and the same shared modules.

- `server.scm`: a Guile web server with a JSON API to manage the tasks,
  which are stored in SQLite. When it starts, it compiles the web page
  (GuileScript) into JavaScript and serves it, with source maps.

- `vanilla/`: the web page, in GuileScript, using the DOM directly (no
  framework, no bundler: the browser loads the compiled ES modules).

- `react/`: the same web page with React, built with Vite and
  `vite-plugin-guilescript` (see [The React version](#the-react-version)).

- `shared/`: GuileScript modules used by both versions of the web page.
  The task validation (`shared/tasks/validation.gs`) is also used by the
  server, which loads it as a Guile module, so the same rules apply in the
  browser and in the server.

## Running it

It needs GuileScript, [guile-json](https://github.com/aconchillo/guile-json)
and [guile-sqlite3](https://notabug.org/guile-sqlite3/guile-sqlite3).

```console
$ guile server.scm
Task manager on http://localhost:8080
```

To run it with GuileScript's source tree instead of an installed
GuileScript, use `GUILE_LOAD_PATH=../.. guile server.scm`.

The `PORT` and `TASKS_DB` environment variables change the port and the
database file (`tasks.db`).

## API

| Request | |
|---|---|
| `GET /api/tasks` | All the tasks. |
| `POST /api/tasks` with `{"title": "..."}` | Create a task. |
| `PATCH /api/tasks/ID` with `{"title": "...", "done": true}` | Change a task (title, done or both). |
| `DELETE /api/tasks/ID` | Delete a task. |

Tasks are objects like `{"id": 1, "title": "Buy milk", "done": false}`.
Invalid requests get a 400 response with the problems, e.g.
`{"errors": ["The title can't be empty."]}`.

## The React version

`react/` is the same task manager with React components, written in
GuileScript and built with Vite. Instead of JSX, the components use an `html`
macro (`react/macros/react/html.scm`, a Guile module):

```scheme
(html (li #:class-name ,(if (.-done task) "task done" "task")
        (input #:type "checkbox" #:checked ,(.-done task))
        (<task-title> #:task task)))
```

Hooks return arrays, which are multiple values in GuileScript:

```scheme
(define-values (tasks set-tasks!) (use-state '()))
```

Run the server (it serves the API) and Vite's development server, which
forwards `/api` to it:

```console
$ guile server.scm
$ cd react
$ npm install
$ npm run dev
```

`npm install` installs `vite-plugin-guilescript` from this repository.
