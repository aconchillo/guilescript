// vite-plugin-guilescript: compile GuileScript (.gs) files with Vite.
//
// Copyright (C) 2026 Aleix Conchillo Flaque <aconchillo@gmail.com>
//
// This file is part of GuileScript.
//
// GuileScript is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
//
// GuileScript is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GuileScript. If not, see https://www.gnu.org/licenses/.

import { execFile } from "node:child_process";
import { resolve } from "node:path";

/**
 * Vite (and Rollup) plugin that compiles GuileScript (.gs) files.
 *
 * Every .gs file is compiled with the guilescript command, and GuileScript
 * modules are imported by the path of their .gs file, so they are compiled
 * by this plugin too. Source maps map the compiled code back to the .gs
 * files.
 *
 * @param {object} [options]
 * @param {string} [options.command] The guilescript command (default: the
 *   GUILESCRIPT environment variable or "guilescript").
 * @param {string[]} [options.loadPath] Directories where GuileScript modules
 *   (and Guile modules, e.g. with macros) are also searched (guilescript -L),
 *   relative to the project root.
 */
export default function guilescript(options = {}) {
  const command = options.command ?? process.env.GUILESCRIPT ?? "guilescript";
  let root = process.cwd();

  return {
    name: "guilescript",

    configResolved(config) {
      root = config.root;
    },

    async transform(_code, id) {
      const [file] = id.split("?", 1);
      if (!file.endsWith(".gs")) {
        return null;
      }

      const loadPath = (options.loadPath ?? []).map((dir) => resolve(root, dir));
      const args = [
        "--source-imports",
        "--source-map",
        ...loadPath.flatMap((dir) => ["-L", dir]),
        file,
      ];
      const { status, stdout, stderr } = await run(command, args);

      // Guile might also print notes (lines starting with ;;;).
      const lines = stderr.split("\n").filter((line) => line && !line.startsWith(";;;"));

      for (const line of lines) {
        const warning = parseMessage(line, "warning");
        if (warning) {
          this.warn({ message: warning.message, id: file, loc: warning.loc });
        }
      }

      if (status !== 0) {
        const error = lines.map((line) => parseMessage(line, "error")).find(Boolean);
        this.error(
          error
            ? { message: error.message, id: file, loc: error.loc }
            : { message: lines.join("\n") || `${command} failed`, id: file },
        );
      }

      return extractSourceMap(stdout, file);
    },

    // In development, Vite only transforms the files it knows are JavaScript
    // (or imported from JavaScript, with ?import), so the .gs files the
    // browser requests directly (e.g. <script type="module"
    // src="/src/main.gs">) are transformed here.
    configureServer(server) {
      server.middlewares.use(async (req, res, next) => {
        const [path, query = ""] = (req.url ?? "").split("?", 2);
        if (!path.endsWith(".gs") || new URLSearchParams(query).has("import")) {
          return next();
        }
        try {
          const result = await server.transformRequest(req.url);
          if (!result) {
            return next();
          }
          res.setHeader("Content-Type", "text/javascript");
          res.end(result.code);
        } catch (error) {
          next(error);
        }
      });
    },
  };
}

const inlineSourceMap =
  /\n\/\/# sourceMappingURL=data:application\/json;charset=utf-8;base64,([A-Za-z0-9+/=]+)\s*$/;

// guilescript writes the source map inline (at the end of the code).
function extractSourceMap(output, file) {
  const match = output.match(inlineSourceMap);
  if (!match) {
    return { code: output, map: null };
  }
  const map = JSON.parse(Buffer.from(match[1], "base64").toString("utf8"));
  map.sources = [file];
  return { code: output.slice(0, match.index + 1), map };
}

function run(command, args) {
  return new Promise((resolve, reject) => {
    execFile(command, args, { maxBuffer: 64 * 1024 * 1024 }, (error, stdout, stderr) => {
      if (error && typeof error.code !== "number") {
        reject(new Error(`vite-plugin-guilescript: could not run ${command}: ${error.message}`));
      } else {
        resolve({ status: error ? error.code : 0, stdout, stderr });
      }
    });
  });
}

// "src/app.gs:3:5: error: message" (lines are 1-based, columns 0-based).
function parseMessage(line, kind) {
  const match = line.match(new RegExp(`^(.*?):(\\d+):(\\d+): ${kind}: (.*)$`));
  return (
    match && {
      message: match[4],
      loc: { file: match[1], line: Number(match[2]), column: Number(match[3]) },
    }
  );
}
