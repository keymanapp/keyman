/*
 * Note:  while this file is not meant to exist long-term, it provides a nice
 * low-level proof-of-concept for esbuild bundling of the various Web submodules.
 *
 * Add some extra code at the end of src/index.ts and run it to verify successful bundling!
 */

import esbuild from 'esbuild';


await esbuild.build({
  alias: {
    'tslib': '@keymanapp/tslib'
  },
  bundle: true,
  format: "esm",
  sourcemap: true,
  target: "es5",
  external: ['fs', 'vm'],
  // nodePaths: ['../../node_modules'],
  entryPoints: {
    'index': 'build/obj/web/index.js',
  },
  outdir: 'build/lib/web',
  outExtension: { '.js': '.mjs' }
});