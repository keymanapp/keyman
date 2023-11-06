/*
 * Note:  while this file is not meant to exist long-term, it provides a nice
 * low-level proof-of-concept for esbuild bundling of the various Web submodules.
 *
 * Add some extra code at the end of src/index.ts and run it to verify successful bundling!
 */

import esbuild from 'esbuild';
import { bundleObjEntryPoints, esmConfiguration, prepareTslibTreeshaking } from '../es-bundling/build/index.mjs';

import fs from 'fs';

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

const embeddedWorkerBuildOptions = await prepareTslibTreeshaking({
  ...esmConfiguration,
  ...bundleObjEntryPoints('lib', 'build/obj/index.js', 'build/obj/worker-main.js'),
  // To be explicit, in comparison to the other build below.  Even if our other
  // configs change their default, we should not minify for THIS build; we'll
  // have a separate minify pass later, after concatenating our polyfills.
  minify: false
  // No treeshaking at the moment (in case it treeshakes out certain model templates!)
});

// Direct-use version
await esbuild.build(embeddedWorkerBuildOptions);

// ------------------------

// Now to generate a filesize profile for the minified version of the worker.
// We want a specialized bundle build here instead; no output, but minified like
// the actual release worker.
const minifiedProfilingOptions = {
  ...embeddedWorkerBuildOptions,
  minify: true,
  keepNames: false, // Do NOT enable - will break under Android 5.0 / Chrome 35 environments!
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
