/*
 * Note:  while this file is not meant to exist long-term, it provides a nice
 * low-level proof-of-concept for esbuild bundling of the various Web submodules.
 *
 * Add some extra code at the end of src/index.ts and run it to verify successful bundling!
 */

import esbuild from 'esbuild';
import { spawn } from 'child_process';

await esbuild.build({
  alias: {
    'tslib': '@keymanapp/tslib'
  },
  bundle: true,
  sourcemap: true,
  format: "esm",
  nodePaths: ['../../node_modules'],
  entryPoints: {
    'index': 'build/obj/web/index.js',
  },
  external: ['fs', 'vm'],
  outdir: 'build/lib/web',
  outExtension: { '.js': '.mjs' },
  tsconfig: 'src/web/tsconfig.json',
  target: "es5"
});

// // Direct-use version
// esbuild.buildSync({
//   bundle: true,
//   sourcemap: true,
//   format: "iife",
//   nodePaths: ['..'],
//   entryPoints: {
//     'worker-main': 'build/obj/worker-main.js'
//   },
//   outdir: 'build/lib',
//   tsconfig: 'tsconfig.json',
//   target: "es5"
// });