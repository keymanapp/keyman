/*
 * Note:  while this file is not meant to exist long-term, it provides a nice
 * low-level proof-of-concept for esbuild bundling of the various Web submodules.
 *
 * Add some extra code at the end of src/index.ts and run it to verify successful bundling!
 */

import esbuild from 'esbuild';

// At present, the lexical model compiler project's configuration, when compiled, relies on `require` - and thus, a
// CommonJS version is necessary until this is resolved.

// Browser / namespace-targeted bundle
esbuild.buildSync({
  entryPoints: ['build/version.inc.js'],
  bundle: true,
  sourcemap: true,
  minify: true,
  format: "cjs",
  keepNames: true,
  // Sets 'common/web' as a root folder for module resolution;
  // this allows the keyman-version and utils imports to resolve.
  //
  // We also need to point it at the nested build output folder to resolve in-project
  // imports when compiled - esbuild doesn't seem to pick up on the shifted base.
  nodePaths: ['..', "build/obj"],
  outfile: "build/version.inc.cjs",
  tsconfig: 'tsconfig.json',
  target: "es5"
});