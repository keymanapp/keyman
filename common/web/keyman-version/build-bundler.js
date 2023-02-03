/*
 * Bundles @keymanapp/keyman-version as a single-file CommonJS module for components
 * still in need of it.
 */

import esbuild from 'esbuild';

// Bundles to a compact ESModule
esbuild.buildSync({
  entryPoints: ['build/version.inc.js'],
  bundle: true,
  sourcemap: true,
  //minify: true,    // No need to minify a module.
  //keepNames: true,
  format: "cjs",
  // Sets 'common/web' as a root folder for module resolution;
  // this allows the keyman-version import to resolve.
  nodePaths: ['..'],
  outfile: "build/version.inc.cjs",
  tsconfig: 'tsconfig.json',
  target: "es5"
});