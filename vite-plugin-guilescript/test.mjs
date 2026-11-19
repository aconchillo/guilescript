// Tests for vite-plugin-guilescript (without Vite: the plugin hooks are
// called directly). The guilescript command is taken from the GUILESCRIPT
// environment variable.

import assert from "node:assert/strict";
import { mkdtempSync, writeFileSync, rmSync, mkdirSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import guilescript from "./index.js";

const dir = mkdtempSync(join(tmpdir(), "vite-plugin-guilescript-"));

function fixture(name, code) {
  const file = join(dir, name);
  writeFileSync(file, code);
  return file;
}

// A minimal plugin context: warn collects warnings, error throws.
const warnings = [];
const context = {
  warn(warning) {
    warnings.push(warning);
  },
  error(error) {
    throw Object.assign(new Error(error.message), error);
  },
};

const plugin = guilescript();

try {
  mkdirSync(join(dir, "app"));
  fixture("app/util.gs", '(define-module (app util) #:export (shout))\n(define (shout s) (string-append s "!"))\n');
  const main = fixture("app/main.gs", '(define-module (app main) #:use-module (app util))\n(console-log (shout "hi"))\n');

  // Other files are not transformed.
  assert.equal(await plugin.transform.call(context, "", join(dir, "app/main.js")), null);

  // GuileScript modules are imported by the path of their .gs file, so the
  // plugin compiles them too.
  const result = await plugin.transform.call(context, "", `${main}?import`);
  assert.match(result.code, /^import \{ shout \} from "\.\/util\.gs";$/m);
  assert.match(result.code, /^console\.log\(shout\("hi"\)\);$/m);
  assert.deepEqual(warnings, []);

  // The source map (removed from the code) maps the code to the .gs file.
  assert.doesNotMatch(result.code, /sourceMappingURL/);
  assert.equal(result.map.version, 3);
  assert.deepEqual(result.map.sources, [main]);
  assert.match(result.map.sourcesContent[0], /\(console-log \(shout "hi"\)\)/);
  // import => line 1, console.log => line 2 (0-based lines 0 and 1 of the .gs file)
  assert.equal(result.map.mappings, "AAAA;AACA");

  // Compilation errors have the location of the error.
  const broken = fixture("broken.gs", "(define (greet name)\n  (consle-log name))\n");
  await assert.rejects(plugin.transform.call(context, "", broken), (error) => {
    assert.match(error.message, /`consle-log' is not defined/);
    assert.equal(error.id, broken);
    assert.deepEqual(error.loc, { file: broken, line: 2, column: 3 });
    return true;
  });

  // A missing guilescript command is reported.
  const missing = guilescript({ command: join(dir, "no-such-guilescript") });
  await assert.rejects(missing.transform.call(context, "", main), /no-such-guilescript not found\. Install GuileScript/);
} finally {
  rmSync(dir, { recursive: true, force: true });
}

console.log("vite-plugin-guilescript: all tests passed");
