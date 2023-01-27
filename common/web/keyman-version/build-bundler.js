/*
 * Bundles @keymanapp/web-utils as single-file modules based upon the export list in src/index.ts.
 */

import esbuild from 'esbuild';
import { spawn } from 'child_process';

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
  outfile: "build/index.cjs",
  tsconfig: 'tsconfig.json',
  target: "es5"
});

const dtsBundleCommand = spawn('npx dts-bundle-generator --project tsconfig.json -o build/index.d.ts version.inc.ts', {
  shell: true
});

dtsBundleCommand.stdout.on('data', data =>   console.log(data.toString()));
dtsBundleCommand.stderr.on('data', data => console.error(data.toString()));

// Forces synchronicity; done mostly so that the logs don't get jumbled up.
dtsBundleCommand.on('exit', () => {
  if(dtsBundleCommand.exitCode != 0) { // Ensure the exit code gets emitted!
    process.exit(dtsBundleCommand.exitCode);
  }
});