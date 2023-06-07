/*
 * Note:  while this file is not meant to exist long-term, it provides a nice
 * low-level proof-of-concept for esbuild bundling of the various Web submodules.
 *
 * Add some extra code at the end of src/index.ts and run it to verify successful bundling!
 */

import esbuild from 'esbuild';
import { spawn } from 'child_process';
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

async function determineNeededDowncompileHelpers(config, log) {
  // Component #1:  Detect all `tslib` helpers we actually want to use.
  const tslibHelperNames = [
    "__extends",
    "__assign",
    "__rest",
    "__decorate",
    "__param",
    "__metadata",
    "__awaiter",
    "__generator",
    "__exportStar",
    "__createBinding",
    "__values",
    "__read",
    "__spread",
    "__spreadArrays",
    "__await",
    "__asyncGenerator",
    "__asyncDelegator",
    "__asyncValues",
    "__makeTemplateObject",
    "__importStar",
    "__importDefault",
    "__classPrivateFieldGet",
    "__classPrivateFieldSet"
  ];

  const detectedHelpers = [];

  let tslibHelperDetectionPlugin = {
    name: 'tslib helper use detection',
    setup(build) {
      build.onLoad({filter: /\.js$/}, async (args) => {
        //
        if(/tslib.js$/.test(args.path)) {
          return;
        }

        let source = await fs.promises.readFile(args.path, 'utf8');

        for(let helper of tslibHelperNames) {
          if(source.indexOf(helper) > -1 && !detectedHelpers.find((entry) => entry == helper)) {
            detectedHelpers.push(helper);
          }
        }

        return;
      });
    }
  }

  // tslib tree-shake phase 1 - detecting which helpers are safe to remove.
  await esbuild.build({
    ...config,
    plugins: [ tslibHelperDetectionPlugin, ...config.plugins ],
    write: false
  });

  // At this point, we can determine what's unused.
  detectedHelpers.sort();

  const unusedHelpers = [];
  tslibHelperNames.forEach((entry) => {
    if(!detectedHelpers.find((detected) => detected == entry)) {
      unusedHelpers.push(entry);
    }
  });

  if(log) {
    // Logs on the tree-shaking decisions
    console.log("Detected helpers from tslib: ");
    console.log();

    console.log();
    console.log("Unused helpers: ");
    console.log(unusedHelpers);
  }

  return unusedHelpers;
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

// tslib tree-shake phase 1 - detecting which helpers are safe to remove.
const unusedHelpers = await determineNeededDowncompileHelpers(commonConfig);

// Component #2:  when we've actually determined which ones are safe to remove, this plugin
// can remove their code.
let tslibForcedTreeshakingPlugin = {
  name: 'tslib helpers - forced treeshaking',
  setup(build) {
    build.onLoad({filter: /tslib.js$/}, async (args) => {
      let source = await fs.promises.readFile(args.path, 'utf8');

      // TODO:  transformations to eliminate the stuff we don't want.
      for(let unusedHelper of unusedHelpers) {
        // Removes the 'exporter' line used to actually export it from the tslib source.
        source = source.replace(`exporter\(\"${unusedHelper}\", ${unusedHelper}\);`, '');

        // Removes the actual helper function definition - obviously, the biggest filesize savings to be had here.
        let definitionStart = source.indexOf(`${unusedHelper} = function`);
        if(definitionStart == -1) {
          if(unusedHelper == '__createBinding') {
            console.warn("Currently unable to force-treeshake the __createBinding tslib helper");
          } else {
            console.error("tslib has likely been updated recently; could not force-treeshake tslib helper " + unusedHelper);
          }
          continue;
        }

        let scopeDepth = 0;
        let i = definitionStart;
        let char = source.charAt(i);
        while(char != '}' || --scopeDepth != 0) {
          if(char == '{') {
            scopeDepth++;
          }
          i++;
          char = source.charAt(i);
        }
        i++; // we want to erase it, too.

        source = source.replace(source.substring(definitionStart, i), '');

        // The top-level var declaration is auto-removed by esbuild when no references to it remain.

      }

      return {
        contents: source,
        loader: 'js'
      };
    });
  }
}

commonConfig.plugins = [tslibForcedTreeshakingPlugin, ...commonConfig.plugins];

// From here, the builds are configured to do phase 2 from the preprocessing data done 'til now.
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