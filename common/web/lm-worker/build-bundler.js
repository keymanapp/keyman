/*
 * Note:  while this file is not meant to exist long-term, it provides a nice
 * low-level proof-of-concept for esbuild bundling of the various Web submodules.
 *
 * Add some extra code at the end of src/index.ts and run it to verify successful bundling!
 */

import esbuild from 'esbuild';
import { esmConfiguration, bundleObjEntryPointsAsLib } from '../es-bundling/build/index.mjs';

import fs from 'fs';
import { determineNeededDowncompileHelpers, buildTslibTreeshaker } from '@keymanapp/tslib/esbuild-tools';

let EMIT_FILESIZE_PROFILE = false;

if(process.argv.length > 2) {
  for(let i = 2; i < process.argv.length; i++) {
    const arg = process.argv[i];

    switch(arg) {
      case '':
        break;
      case '--ci':
        EMIT_FILESIZE_PROFILE=true
        break;
      // May add other options if desired in the future.
      default:
        console.error("Invalid command-line option set for script; only --ci is permitted.");
        process.exit(1);
    }
  }
}

/** @type {esbuild.BuildOptions} */
const embeddedWorkerBuildOptions = {
  ...esmConfiguration,
  ...bundleObjEntryPointsAsLib('build/obj/index.js', 'build/obj/worker-main.js')
};

// Prepare the needed setup for `tslib` treeshaking.
const unusedHelpers = await determineNeededDowncompileHelpers(embeddedWorkerBuildOptions);
embeddedWorkerBuildOptions.plugins = [buildTslibTreeshaker(unusedHelpers), ...embeddedWorkerBuildOptions.plugins];

// Direct-use version
await esbuild.build(embeddedWorkerBuildOptions);

// ------------------------

// Now to generate a filesize profile for the minified version of the worker.
// We want a specialized bundle build here instead; no output, but minified like
// the actual release worker.
const minifiedProfilingOptions = {
  ...embeddedWorkerBuildOptions,
  minify: true,
  metafile: true,
  write: false // don't actually write the file.
}

let result = await esbuild.build(minifiedProfilingOptions);
let filesizeProfile = await esbuild.analyzeMetafile(result.metafile, { verbose: true });
fs.writeFileSync('build/filesize-profile.log', `
// Minified Worker filesize profile, before polyfilling
${filesizeProfile}
`);

if(EMIT_FILESIZE_PROFILE) {
  console.log("Minified, pre-polyfill worker filesize profile:");
  // Profiles the sourcecode!
  console.log(filesizeProfile);
}
