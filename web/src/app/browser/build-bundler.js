/*
 * Note:  while this file is not meant to exist long-term, it provides a nice
 * low-level proof-of-concept for esbuild bundling of the various Web submodules.
 *
 * Add some extra code at the end of src/index.ts and run it to verify successful bundling!
 */

import esbuild from 'esbuild';
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

// Component #1:  Detect all `tslib` helpers we actually want to use.
async function determineNeededDowncompileHelpers(config, log) {
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
          // Returning `undefined` makes this 'pass-through' - it doesn't prevent other
          // configured plugins from working.
          return;
        }

        let source = await fs.promises.readFile(args.path, 'utf8');

        for(let helper of tslibHelperNames) {
          if(source.indexOf(helper) > -1 && !detectedHelpers.find((entry) => entry == helper)) {
            detectedHelpers.push(helper);
          }
        }

        // Returning `undefined` makes this 'pass-through' - it doesn't prevent other
        // configured plugins from working.
        return;
      });
    }
  }

  // tslib tree-shake phase 1 - detecting which helpers are safe to remove.
  await esbuild.build({
    ...config,
    // `tslibHelperDetectionPlugin` is pass-through, and so has no net effect on
    // the build.  We just use this run to scan for any utilized `tslib` helper funcs,
    // not to manipulate the actual source in any way.
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

// Component #2:  when we've actually determined which ones are safe to remove, this plugin
// can remove their code.
function configuredDowncompileTreeshakePlugin(unusedHelpers) {
  function indexToSourcePosition(source, index) {
    let priorText = source.substring(0, index);
    let lineNum = priorText.split('\n').length;
    let lastLineBreakIndex = priorText.lastIndexOf('\n');
    let colNum = index - lastLineBreakIndex;
    let nextLineBreakIndex = source.indexOf('\n', lastLineBreakIndex+1);

    return {
      line: lineNum,
      column: colNum,
      lineText: source.substring(lastLineBreakIndex+1, nextLineBreakIndex)
    }
  }

  return {
    name: 'tslib forced treeshaking',
    setup(build) {
      build.onLoad({filter: /tslib.js$/}, async (args) => {
        const trueSource = await fs.promises.readFile(args.path, 'utf8');
        let source = trueSource;

        let warnings = [];
        let errors = [];

        for(let unusedHelper of unusedHelpers) {
          // Removes the 'exporter' line used to actually export it from the tslib source.
          source = source.replace(`exporter\(\"${unusedHelper}\", ${unusedHelper}\);`, '');

          // Removes the actual helper function definition - obviously, the biggest filesize savings to be had here.
          let definitionStart = source.indexOf(`${unusedHelper} = function`);

          // Emission of warnings & errors
          if(definitionStart == -1) {
            let matchString = `${unusedHelper} =`
            let bestGuessIndex = trueSource.indexOf(matchString);
            if(bestGuessIndex == -1) {
              matchString = `var ${unusedHelper}`
              bestGuessIndex = trueSource.indexOf(matchString);
            }

            if(bestGuessIndex == -1) {
              matchString = '';
            }

            let logLocation = indexToSourcePosition(trueSource, bestGuessIndex);

            let location = {
              file: args.path,
              line: logLocation.line,
              column: logLocation.column,
              length: matchString.length,
              lineText: logLocation.lineText
            }

            if(unusedHelper == '__createBinding') {
              warnings.push({
                text: "Currently unable to force-treeshake the __createBinding tslib helper",
                location: location
              });
            } else {
              warnings.push({
                text: "tslib has likely been updated recently; could not force-treeshake tslib helper " + unusedHelper,
                location: location
              });
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
          loader: 'js',
          warnings: warnings,
          errors: errors
        };
      });
    }
  };
};

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
const unusedHelpers = await determineNeededDowncompileHelpers(commonConfig);
commonConfig.plugins = [configuredDowncompileTreeshakePlugin(unusedHelpers), ...commonConfig.plugins];

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