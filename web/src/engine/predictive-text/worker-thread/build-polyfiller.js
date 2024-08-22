import fs from 'fs';

import SourceMapCombiner from 'combine-source-map';
import convertSourceMap from 'convert-source-map'; // Transforms sourcemaps among various common formats.
                                                   // Base64, stringified-JSON, end-of-file comment...

import esbuild from 'esbuild';

let sourceFromArgs;
let destFromArgs;

function doHelp(errMessage) {
  if(errMessage) {
    console.error(errMessage + '\n');
  }
  console.log(`
Summary:
  Creates a polyfilled version of the lm-worker to ensure compatibility with the needs of
  Keyman for Android when run on Android 5.0 / API 21 without an updated Chrome webview.

Usage:
  node build-polyfiller.js <input-file> [options...]

Parameters:
  <input-file>:  Fully-bundled and compiled JS file to be polyfilled.

Options:
  --help            Shows this script's documentation
  --out <out-file>  Specifies the destination path for the polyfilled output.

                    If missing, the output will be placed next to the source and given
                    the same path, but with '.polyfilled.js' replacing the original '.js'
                    extension.

                    Either way, a minified version will also be output, using the same
                    path but with the final '.js' replaced by '.min.js'.
` );
  process.exit(errMessage ? 1 : 0);
}

if(process.argv.length > 2) {
  for(let i = 2; i < process.argv.length; i++) {
    const arg = process.argv[i];

    switch(arg) {
      case '--help':
        doHelp();
        break;
      case '--out':
        destFromArgs = process.argv[++i];
        break;
      default:
        if(!sourceFromArgs) {
          sourceFromArgs = arg;
        } else {
          doHelp("Input file can only be specified once; aborting");
        }
    }
  }
} else {
  // Display help + abort.
  doHelp("Required parameters missing");
}

if(!sourceFromArgs || sourceFromArgs.substring(sourceFromArgs.length - 3) != '.js') {
  doHelp("No input file has been specified; aborting.");
}

const sourceFile = sourceFromArgs;
const destFile = destFromArgs || sourceFromArgs.substring(0, sourceFromArgs.length - 3) + '.polyfilled.js';
const minDestFile = destFile.substring(0, destFile.length - 3) + '.min.js';

let loadPolyfill = function(scriptFile, sourceMapFile) {
  // May want to retool the pathing somewhat!
  return {
    source: fs.readFileSync(scriptFile).toString(),
    sourceFile: sourceMapFile || scriptFile
  };
}

let loadCompiledModuleFilePair = function(file, mapFilename) {
  let module = fs.readFileSync(file);
  let moduleSourcemapJSON = JSON.parse(fs.readFileSync(file + '.map').toString());

  // May want to retool the pathing somewhat!
  return {
    plainSource: `${module}`,
    sourceMapAsJSON: moduleSourcemapJSON,
    sourceFile: mapFilename || file,

    /**The source + inlined sourcemap-as-comment used by `combine-source-map`. */
    get source() {
      let jsonAsBuffer = Buffer.from(JSON.stringify(this.sourceMapAsJSON));
      return `${this.plainSource}\n${convertSourceMap.fromJSON(jsonAsBuffer).toComment()}`;
    }
  };
}

function concatScriptsAndSourcemaps(files, finalName, separatorFile) {
  let combiner = SourceMapCombiner.create(finalName);

  let finalConcatenationArray = [];
  let lineCountThusFar = 0;

  for(let filePairing of files) {
    let offset = {
      line: lineCountThusFar
    };

    console.log(`- ${filePairing.sourceFile}`);
    combiner = combiner.addFile(filePairing, offset);

    let rawSourceToConcat = filePairing.plainSource || filePairing.source;
    lineCountThusFar += rawSourceToConcat.split('\n').length + 1; // Not sure why it needs the fudge-factor, but it does.

    finalConcatenationArray.push(filePairing);
    if(filePairing != files[files.length-1]) {
      combiner = combiner.addFile(separatorFile);
      finalConcatenationArray.push(separatorFile);
    }
  }

  let bundledSource = finalConcatenationArray.map((pair => pair.plainSource || pair.source)).join('');

  return {
    script: bundledSource,
    sourcemapJSON: JSON.parse(convertSourceMap.fromBase64(combiner.base64()).toJSON()),
    scriptFilename: finalName
  }
}

// Centralized?

console.log("Pass 1:  worker + polyfill concatenation");

let separatorFile = {
  source: `

  `,
  sourceFile: '<inline>'
}

let sourceFileSet = [
  // Needed for Android / Chromium browser pre-41.
  loadPolyfill('../../../node_modules/string.prototype.codepointat/codepointat.js', 'src/polyfills/string.codepointat.js'),
  // Needed for Android / Chromium browser pre-45.
  // Not used in this codebase, but used by some compiled model defaults.
  loadPolyfill('src/polyfills/array.from.js', 'src/polyfills/array.from.js'),
  // Needed for Android / Chromium browser pre-45.
  loadPolyfill('src/polyfills/array.fill.js', 'src/polyfills/array.fill.js'),
  // Needed for Android / Chromium browser pre-45.
  loadPolyfill('src/polyfills/array.findIndex.js', 'src/polyfills/array.findIndex.js'),
  // Needed for Android / Chromium browser pre-47.
  loadPolyfill('src/polyfills/array.includes.js', 'src/polyfills/array.includes.js'),
  // For Object.values, for iteration over object-based associate arrays.
  // Needed for Android / Chromium browser pre-54.
  loadPolyfill('src/polyfills/object.values.js', 'src/polyfills/object.values.js'),
  // Needed to support Symbol.iterator, as used by the correction algorithm.
  // Needed for Android / Chromium browser pre-43.
  loadPolyfill('src/polyfills/symbol-es6.min.js', 'src/polyfills/symbol-es6.min.js'),
  loadCompiledModuleFilePair(sourceFile, sourceFile)
];

let fullWorkerConcatenation = concatScriptsAndSourcemaps(sourceFileSet, destFile, separatorFile);

// New stage:  cleaning the sourcemaps

// Sources are being passed into the sourcemap concatenator via our working directory.
let sourceRoot = '@keymanapp/keyman/web/src/engine/predictive-text/worker-thread/';
fullWorkerConcatenation.sourcemapJSON.sourceRoot = sourceRoot;

// End "cleaning the sourcemaps"

console.log();
console.log("Pass 2:  Output intermediate state and perform minification");

// IMPORTANT: Remove file-end sourcemap ref comment and replace it!
fullWorkerConcatenation.script = fullWorkerConcatenation.script.substring(0, fullWorkerConcatenation.script.lastIndexOf('//# sourceMappingURL'));
fullWorkerConcatenation.script += `//# sourceMappingURL=${fullWorkerConcatenation.scriptFilename}.map`;

fs.writeFileSync(fullWorkerConcatenation.scriptFilename, fullWorkerConcatenation.script);
if(fullWorkerConcatenation.sourcemapJSON) {
  fs.writeFileSync(`${fullWorkerConcatenation.scriptFilename}.map`, convertSourceMap.fromObject(fullWorkerConcatenation.sourcemapJSON).toJSON());
}

await esbuild.build({
  entryPoints: [destFile],
  sourcemap: 'external',
  sourcesContent: true,
  minify: true,
  // Do NOT enable - will break under Android 5.0 / Chrome 35 environments, likely through Chrome 42.
  // https://caniuse.com/mdn-javascript_builtins_function_name_configurable_true
  keepNames: false,
  target: 'es6',
  outfile: minDestFile
});
