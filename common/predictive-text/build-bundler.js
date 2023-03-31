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
  nodePaths: ['../../node_modules'],
  entryPoints: {
    'index': 'build/obj/web/index.js',
  },
  external: ['fs', 'vm'],
  outdir: 'build/lib/web',
  outExtension: { '.js': '.mjs' },
  tsconfig: 'src/web/tsconfig.json',
  target: "es5"
});

// // Direct-use version
// esbuild.buildSync({
//   bundle: true,
//   sourcemap: true,
//   format: "iife",
//   nodePaths: ['..'],
//   entryPoints: {
//     'worker-main': 'build/obj/worker-main.js'
//   },
//   outdir: 'build/lib',
//   tsconfig: 'tsconfig.json',
//   target: "es5"
// });

const dtsBundleCommand1 = spawn('npx dts-bundle-generator --project src/web/tsconfig.json -o build/lib/web/index.d.ts src/web/index.ts', {
  shell: true
});

dtsBundleCommand1.stdout.on('data', data =>   console.log(data.toString()));
dtsBundleCommand1.stderr.on('data', data => console.error(data.toString()));

// Forces synchronicity; done mostly so that the logs don't get jumbled up.
await new Promise((resolve, reject) => {
  dtsBundleCommand1.on('exit', () => {
    if(dtsBundleCommand1.exitCode != 0) {
      reject();
      process.exit(dtsBundleCommand1.exitCode);
    }
    resolve();
  });
});

