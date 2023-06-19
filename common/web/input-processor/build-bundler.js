/*
 * Note:  while this file is not meant to exist long-term, it provides a nice
 * low-level proof-of-concept for esbuild bundling of the various Web submodules.
 *
 * Add some extra code at the end of src/index.ts and run it to verify successful bundling!
 */

import esbuild from 'esbuild';

// Bundled ES module version
esbuild.buildSync({
  entryPoints: ['build/obj/index.js'],
  alias: {
    'tslib': '@keymanapp/tslib'
  },
  bundle: true,
  sourcemap: true,
  external: ['fs', 'vm'],
  format: "esm",
  // Sets 'common/web' as a root folder for module resolution;
  // this allows the keyman-version and utils imports to resolve.
  //
  // We also need to point it at the nested build output folder to resolve in-project
  // imports when compiled - esbuild doesn't seem to pick up on the shifted base.
  nodePaths: ['..', "build/obj"],
  outfile: "build/lib/index.mjs",
  tsconfig: 'tsconfig.json',
  target: "es5"
});

// Bundled CommonJS (classic Node) module version
esbuild.buildSync({
  entryPoints: ['build/obj/index.js'],
  alias: {
    'tslib': '@keymanapp/tslib'
  },
  bundle: true,
  sourcemap: true,
  external: ['fs', 'vm'],
  format: "cjs",
  // Sets 'common/web' as a root folder for module resolution;
  // this allows the keyman-version and utils imports to resolve.
  //
  // We also need to point it at the nested build output folder to resolve in-project
  // imports when compiled - esbuild doesn't seem to pick up on the shifted base.
  nodePaths: ['..', "build/obj"],
  outfile: "build/lib/index.cjs",
  tsconfig: 'tsconfig.json',
  target: "es5"
});