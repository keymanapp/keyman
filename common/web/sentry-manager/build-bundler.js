/*
 * Note:  while this file is not meant to exist long-term, it provides a nice
 * low-level proof-of-concept for esbuild bundling of the various Web submodules.
 *
 * Add some extra code at the end of src/index.ts and run it to verify successful bundling!
 */

import esbuild from 'esbuild';

await esbuild.build({
  bundle: true,
  sourcemap: true,
  format: "iife",
  nodePaths: ['../../../node_modules'],
  entryPoints: {
    'index': 'build/obj/index.js',
  },
  outfile: 'build/lib/index.js',
  target: "es5",
  tsconfig: './src/tsconfig.json',
  sourceRoot: '@keymanapp/keyman/common/web/sentry-manager/build/lib/'
});