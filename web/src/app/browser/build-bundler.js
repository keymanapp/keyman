/*
 * Note:  while this file is not meant to exist long-term, it provides a nice
 * low-level proof-of-concept for esbuild bundling of the various Web submodules.
 *
 * Add some extra code at the end of src/index.ts and run it to verify successful bundling!
 */

import esbuild from 'esbuild';
import fs from 'fs';
import { esmConfiguration, iifeConfiguration, prepareTslibTreeshaking } from '../../../../common/web/es-bundling/build/index.mjs';

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

const commonConfig = {
  ...iifeConfiguration,
  entryPoints: {
    'index': '../../../build/app/browser/obj/debug-main.js',
  },
  outfile: '../../../build/app/browser/debug/keymanweb.js',
  // `esbuild`'s sourcemap output puts relative paths to the original sources from the
  // directory of the build output.  The following keeps repo structure intact and
  // puts our code under a common 'namespace' of sorts.
  sourceRoot: '@keymanapp/keyman/web/build/app/browser/debug/'
};

await prepareTslibTreeshaking(commonConfig, /worker-main\.wrapped(?:\.min)?\.js/);

// And now... do the actual builds.
await esbuild.build(commonConfig);

let result = await esbuild.build({
  ...commonConfig,
  entryPoints: {
    'index': '../../../build/app/browser/obj/release-main.js',
  },
  // Enables source-file output size profiling!
  metafile: true,
  minify: true,
  outfile: '../../../build/app/browser/release/keymanweb.js',
});

let filesizeProfile = await esbuild.analyzeMetafile(result.metafile, { verbose: true });
fs.writeFileSync('../../../build/app/browser/filesize-profile.log', `
// Minified Keyman Engine for Web ('app/browser' target), filesize profile
${filesizeProfile}
`);
if(EMIT_FILESIZE_PROFILE) {
  // Profiles the sourcecode!
  console.log(filesizeProfile);
}

await esbuild.build({
  ...esmConfiguration,
  entryPoints: {
    'index': '../../../build/app/browser/obj/test-index.js',
  },
  outfile: '../../../build/app/browser/lib/index.mjs'
});