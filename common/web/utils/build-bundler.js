/*
 * Note:  while this file is not meant to exist long-term, it provides a nice
 * low-level proof-of-concept for esbuild bundling of the various Web submodules.
 *
 * Add some extra code at the end of src/index.ts and run it to verify successful bundling!
 */

import esbuild from 'esbuild';

esbuild.buildSync({
  entryPoints: ['build/modules/index.js'],
  bundle: true,
  sourcemap: true,
  minify: true,
  keepNames: true,
  // Sets 'common/web' as a root folder for module resolution;
  // this allows the keyman-version import to resolve.
  nodePaths: ['..'],
  outfile: "build/bundled/index.js",
  tsconfig: 'tsconfig.json',
  target: "es5"
});