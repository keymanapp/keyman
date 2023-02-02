/*
 * Note:  while this file is not meant to exist long-term, it provides a nice
 * low-level proof-of-concept for esbuild bundling of the various Web submodules.
 *
 * Add some extra code at the end of src/index.ts and run it to verify successful bundling!
 */

import esbuild from 'esbuild';
import { spawn } from 'child_process';

await esbuild.build({
  bundle: true,
  sourcemap: true,
  format: "esm",
  nodePaths: ['..'],
  entryPoints: ['build/obj/index.js'],
  outfile: "build/lib/index.mjs",
  tsconfig: 'tsconfig.json',
  target: "es5"
});


// Bundled CommonJS (classic Node) module version
esbuild.buildSync({
  bundle: true,
  sourcemap: true,
  format: "cjs",
  nodePaths: ['..'],
  entryPoints: ['build/obj/index.js'],
  outfile: "build/lib/index.cjs",
  tsconfig: 'tsconfig.json',
  target: "es5"
});