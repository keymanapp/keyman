/*
 * Bundles @keymanapp/web-utils as single-file modules based upon the export list in src/index.ts.
 */

import esbuild from 'esbuild';
import { spawn } from 'child_process';

// Bundles to a compact ESModule
esbuild.buildSync({
  entryPoints: ['build/obj/index.js'],
  bundle: true,
  sourcemap: true,
  //minify: true,    // No need to minify a module.
  //keepNames: true,
  format: "esm",
  // Sets 'common/web' as a root folder for module resolution;
  // this allows the keyman-version import to resolve.
  nodePaths: ['..'],
  outfile: "build/lib/index.mjs",
  tsconfig: 'tsconfig.json',
  target: "es5"
});

// Bundles to a compact CommonJS (classic Node) module
esbuild.buildSync({
  entryPoints: ['build/obj/index.js'],
  bundle: true,
  sourcemap: true,
  //minify: true,    // No need to minify a module.
  //keepNames: true,
  format: "cjs",
  // Sets 'common/web' as a root folder for module resolution;
  // this allows the keyman-version import to resolve.
  nodePaths: ['..'],
  outfile: "build/lib/index.cjs",
  tsconfig: 'tsconfig.json',
  target: "es5"
});

const dtsBundleCommand = spawn('npx dts-bundle-generator --project tsconfig.json -o build/lib/index.d.ts src/index.ts', {
  shell: true
});

dtsBundleCommand.stdout.on('data', data =>   console.log(data.toString()));
dtsBundleCommand.stderr.on('data', data => console.error(data.toString()));