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
  // this allows the keyman-version and utils imports to resolve.
  //
  // We also need to point it at the nested build output folder to resolve in-project
  // imports when compiled - esbuild doesn't seem to pick up on the shifted base.
  nodePaths: ['..', "build/modules"],
  outfile: "build/bundled/index.js",
  tsconfig: 'tsconfig.json',
  target: "es5"
});