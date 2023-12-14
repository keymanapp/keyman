/*
 * Bundle kmc as esm with appropriate node modules and banner
 */

import esbuild from 'esbuild';

await esbuild.build({
  entryPoints: [
    'build/src/kmc.js',
    'build/src/kmlmc.js',
    'build/src/kmlmp.js',
  ],
  bundle: true,
  format: 'esm',
  platform: 'node',
  target: 'es2022',
  outdir: 'build/dist/',
  sourcemap: true,

  // We want a .mjs extension to force node into ESM module mode
  outExtension: { '.js': '.mjs' },

  // Thunk for external modules such as Commander that are still CJS and still
  // use require, __filename, __dirname
  banner: {
    js: `
      const require = (await import("node:module")).createRequire(import.meta.url);
      const __filename = (await import("node:url")).fileURLToPath(import.meta.url);
      const __dirname = (await import("node:path")).dirname(__filename);
    `,
  },
});
