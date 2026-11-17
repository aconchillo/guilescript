# vite-plugin-guilescript

A [Vite](https://vite.dev/) plugin to use [GuileScript](https://github.com/aconchillo/guilescript)
(`.gs`) files in Vite projects.

It needs the `guilescript` command (GuileScript 1.0.0 or newer).

```console
$ npm install --save-dev vite-plugin-guilescript
```

```js
// vite.config.js
import { defineConfig } from "vite";
import guilescript from "vite-plugin-guilescript";

export default defineConfig({
  plugins: [guilescript()],
});
```

Then `.gs` files can be used like any other module, e.g. from `index.html`:

```html
<script type="module" src="/src/main.gs"></script>
```

or from JavaScript:

```js
import { makeCounterButton } from "./counter.gs";
```

Every `.gs` file is compiled with `guilescript`. GuileScript modules imported
from a `.gs` file (with `define-module` or `use-modules`) are imported by the
path of their `.gs` file, so they are compiled by the plugin too. Compilation
errors are shown with their location in Vite's error overlay, and source maps
map the compiled code back to the `.gs` files (in development and, with
`build.sourcemap`, in production builds).

## Options

- `command`: the `guilescript` command (default: the `GUILESCRIPT`
  environment variable or `guilescript`).

- `loadPath`: directories where GuileScript modules (and Guile modules, e.g.
  with macros) are also searched (`guilescript -L`), relative to the project
  root.

See `examples/vite` in the GuileScript repository for a complete example.
