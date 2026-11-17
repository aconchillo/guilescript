import { defineConfig } from "vite";
import guilescript from "vite-plugin-guilescript";

export default defineConfig({
  // The shared GuileScript modules are in ../shared and the html macro (a
  // Guile module) in macros.
  plugins: [guilescript({ loadPath: ["../shared", "macros"] })],
  server: {
    // The task API is served by ../server.scm.
    proxy: { "/api": "http://localhost:8080" },
    // ../shared and ../style.css are outside of this directory.
    fs: { allow: [".."] },
  },
});
