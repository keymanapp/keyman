/*
 * Note:  while this file is not meant to exist long-term, it provides a nice
 * low-level proof-of-concept for esbuild bundling of the various Web submodules.
 *
 * Add some extra code at the end of src/index.ts and run it to verify successful bundling!
 */

import esbuild from 'esbuild';
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

/*
 * Refer to https://github.com/microsoft/TypeScript/issues/13721#issuecomment-307259227 -
 * the `@class` emit comment-annotation is designed to facilitate tree-shaking for ES5-targeted
 * down-level emits.  `esbuild` doesn't look for it by default... but we can override that with
 * this plugin.
 */
let es5ClassAnnotationAsPurePlugin = {
  name: '@class -> __PURE__',
  setup(build) {
    build.onLoad({filter: /\.js$/ }, async (args) => {
      let source = await fs.promises.readFile(args.path, 'utf8');
      return {
        // Marks any classes compiled by TS (as per the /** @class */ annotation)
        // as __PURE__ in order to facilitate tree-shaking.
        contents: source.replace('/** @class */', '/* @__PURE__ */ /** @class */'),
        loader: 'js'
      }
    });
  }
}

const commonConfig = {
  alias: {
    'tslib': '@keymanapp/tslib'
  },
  bundle: true,
  sourcemap: true,
  format: "iife",
  nodePaths: ['../../../../node_modules'],
  entryPoints: {
    'index': '../../../build/app/browser/obj/debug-main.js',
  },
  outfile: '../../../build/app/browser/debug/keymanweb.js',
  plugins: [ es5ClassAnnotationAsPurePlugin ],
  target: "es5",
  treeShaking: true,
  tsconfig: './tsconfig.json'
};

// Prepare the needed setup for `tslib` treeshaking.
const unusedHelpers = await determineNeededDowncompileHelpers(commonConfig, /worker-main\.wrapped(?:\.min)?\.js/);
commonConfig.plugins = [buildTslibTreeshaker(unusedHelpers), ...commonConfig.plugins];

// And now... do the actual builds.
await esbuild.build(commonConfig);

let result = await esbuild.build({
  ...commonConfig,
  minifyWhitespace: true,
  minifySyntax: true,
  minifyIdentifiers: true,
  format: "iife",
  nodePaths: ['../../../../node_modules'],
  entryPoints: {
    'index': '../../../build/app/browser/obj/release-main.js',
  },
  outfile: '../../../build/app/browser/release/keymanweb.js',
  // Enables source-file output size profiling!
  metafile: true
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
  ...commonConfig,
  format: "esm",
  entryPoints: {
    'index': '../../../build/app/browser/obj/test-index.js',
  },
  outfile: '../../../build/app/browser/lib/index.mjs',
  tsconfig: './tsconfig.json'
});