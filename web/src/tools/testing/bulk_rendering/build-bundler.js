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
  nodePaths: [
    '../../../../build/tools/testing/bulk_rendering/obj'
  ],
  entryPoints: {
    'index': '../../../../build/tools/testing/bulk_rendering/obj/renderer_core.js',
  },
  outfile: '../../../../build/tools/testing/bulk_rendering/lib/bulk_render.js',
  tsconfig: './tsconfig.json',
  target: "es5",
  treeShaking: true
});
