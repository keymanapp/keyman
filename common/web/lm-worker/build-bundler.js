/*
 * Note:  while this file is not meant to exist long-term, it provides a nice
 * low-level proof-of-concept for esbuild bundling of the various Web submodules.
 *
 * Add some extra code at the end of src/index.ts and run it to verify successful bundling!
 */

import esbuild from 'esbuild';
import { spawn } from 'child_process';
import fs from 'fs';

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

await esbuild.build({
  bundle: true,
  sourcemap: true,
  /*
   * https://esbuild.github.io/api/#sources-content would theoretically allow us to strip the source
   * while still keeping info useful for stack-tracing... but it doesn't pass through the sourcemap
   * concatenation setup.
   *
   * That said, we know how to 'nix it ourselves in post now, so... yeah.
   */
  sourcesContent: true,
  sourceRoot: "/",
  format: "esm",
  nodePaths: ['..', '../../models'],
  entryPoints: {
    'index': 'build/obj/index.js',
    'worker-main': 'build/obj/worker-main.js'
  },
  outdir: 'build/lib',
  outExtension: { '.js': '.mjs' },
  plugins: [ es5ClassAnnotationAsPurePlugin ],
  tsconfig: 'tsconfig.json',
  target: "es5",
});

// Bundled CommonJS (classic Node) module version
await esbuild.build({
  bundle: true,
  sourcemap: true,
  format: "cjs",
  nodePaths: ['..'],
  entryPoints: {
    'index': 'build/obj/index.js',
    'worker-main': 'build/obj/worker-main.js'
  },
  outdir: 'build/lib',
  outExtension: { '.js': '.cjs' },
  plugins: [ es5ClassAnnotationAsPurePlugin ],
  tsconfig: 'tsconfig.json',
  target: "es5"
});

const embeddedWorkerBuildOptions = {
  bundle: true,
  sourcemap: true,
  format: "iife",
  nodePaths: ['..'],
  entryPoints: {
    'worker-main': 'build/obj/worker-main.js'
  },
  outdir: 'build/lib',
  plugins: [ es5ClassAnnotationAsPurePlugin ],
  tsconfig: 'tsconfig.json',
  target: "es5"
}

// Direct-use version
await esbuild.build(embeddedWorkerBuildOptions);

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
