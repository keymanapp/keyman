import fs from 'fs';
import path from 'path';
import esbuild from 'esbuild';

import SourcemapRemapper from '@keymanapp/sourcemap-path-remapper';

import SourcemapCombiner from 'combine-source-map';
import convertSourcemap from 'convert-source-map'; // Transforms sourcemaps among various common formats.
                                                   // Base64, stringified-JSON, end-of-file comment...

let DEBUG = false;
let MINIFY = false;

if(process.argv.length > 2) {
  for(let i = 2; i < process.argv.length; i++) {
    const arg = process.argv[i];

    switch(arg) {
      case '--debug':
        DEBUG = true;
        break;
      case '--minify':
        MINIFY = true;
        break;
      // May add other options if desired in the future.
      default:
        console.error("Invalid command-line option set for script; only --debug and --minify are permitted.");
        process.exit(1);
    }
  }
}

let loadPolyfill = function(file, mapFilename) {
  // May want to retool the pathing somewhat!
  return {
    source: fs.readFileSync(file).toString(),
    sourceFile: mapFilename || file
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
      return `${this.plainSource}\n${convertSourcemap.fromJSON(jsonAsBuffer).toComment()}`;
    }
  };
}

function concatScriptsAndSourcemaps(files, finalName, separatorFile) {
  let combiner = SourcemapCombiner.create(finalName);

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
    sourcemapJSON: JSON.parse(convertSourcemap.fromBase64(combiner.base64()).toJSON()),
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
  // Needed for Android / Chromium browser pre-45.
  loadPolyfill('build/polyfills/array.fill.js', 'polyfills/array.fill.js'),
  // Needed for Android / Chromium browser pre-45.
  loadPolyfill('build/polyfills/array.findIndex.js', 'polyfills/array.findIndex.js'),
  // Needed for Android / Chromium browser pre-45.
  loadPolyfill('build/polyfills/array.from.js', 'polyfills/array.from.js'),
  // Needed for Android / Chromium browser pre-47.
  loadPolyfill('build/polyfills/array.includes.js', 'polyfills/array.includes.js'),
  // For Object.values, for iteration over object-based associate arrays.
  // Needed for Android / Chromium browser pre-54.
  loadPolyfill('build/polyfills/object.values.js', 'polyfills/object.values.js'),
  // Needed to support Symbol.iterator, as used by the correction algorithm.
  // Needed for Android / Chromium browser pre-43.
  loadPolyfill('build/polyfills/symbol-es6.min.js', 'polyfills/symbol-es6.min.js'),
  loadCompiledModuleFilePair('build/lib/worker-main.mjs', 'worker-main.mjs'),
];

let fullWorkerConcatenation = concatScriptsAndSourcemaps(sourceFileSet, "worker-main.polyfilled.js", separatorFile);

// New stage:  cleaning the sourcemaps
console.log();

// Because we're compiling the main project based on its TS build outputs, and our cross-module references
// also link to build outputs, we need to clean up the sourcemap-paths.  Also, the individual module sourcemaps'
// paths result in unwanted extra pathing that needs to be cleaned up (models/models, correction/correction, etc)
console.log("Pass 2:  cleaning sourcemap source paths");

let remappingState = SourcemapRemapper
  .fromObject(fullWorkerConcatenation.sourcemapJSON)
  .remapPaths([
    {from: /^polyfills\//, to: '/common/web/lm-worker/src/polyfills/'},
    {from: 'models/templates/build/obj/', to :'common/models/templates/src/'},
    {from: 'models/wordbreakers/build/obj/default/default', to: 'common/models/wordbreakers/src/default/'},
    {from: 'models/wordbreakers/build/obj/', to: 'common/models/wordbreakers/src/'},
    {from: 'obj/models/models/', to: 'common/web/lm-worker/src/models/'},
    {from: 'obj/correction/correction/', to: 'common/web/lm-worker/src/correction/'},
    {from: 'obj/', to: 'common/web/lm-worker/src/'},
    {from: /^\/utils\/src\//, to: '/common/web/utils/src/'},
    {from: '/keyman-version/', to: '/common/web/keyman-version/'},
    // {from: /^\//, to: ''} // To avoid the later minification pass mangling the paths.
  ], (from, to) => console.log(`- ${from} => ${to}`));

if(remappingState.unchangedSourcepaths.length > 0) {
  console.log();
  console.log("Not mapped:");

  for(let path of remappingState.unchangedSourcepaths) {
    console.log(`- ${path}`);
  }
}

console.log();
let sourceRoot = "/@keymanapp/keyman";
console.log(`Setting sourceRoot: ${sourceRoot}`)
remappingState.sourceRoot = sourceRoot;
fullWorkerConcatenation.sourcemapJSON = remappingState.sourcemap;

// End "cleaning the sourcemaps"

// NOTE:  At this stage, if desired, the JSON form of the sourcemap may be cleaned, source paths altered, etc
// before proceeding!

if(!DEBUG) {
  // Nuke the source text entries entirely if not in DEBUG mode; that's a LOT of extra text to track,
  // which means filesize bloat.
  //
  // We should still get stack traces in any related Sentry logs; we're just keeping the source itself out.
  delete fullWorkerConcatenation.sourcemapJSON.sourcesContent;
}

console.log();
console.log("Pass 3:  Output intermediate state and perform minification");

// IMPORTANT: Remove file-end sourcemap ref comment and replace it!
fullWorkerConcatenation.script = fullWorkerConcatenation.script.substring(0, fullWorkerConcatenation.script.lastIndexOf('//# sourceMappingURL'));
fullWorkerConcatenation.script += `//# sourceMappingURL=${fullWorkerConcatenation.scriptFilename}.map`;

fs.writeFileSync(`build/lib/${fullWorkerConcatenation.scriptFilename}`, fullWorkerConcatenation.script);
fs.writeFileSync(`build/lib/${fullWorkerConcatenation.scriptFilename}.map`, convertSourcemap.fromObject(fullWorkerConcatenation.sourcemapJSON).toJSON());

await esbuild.build({
  entryPoints: [`build/lib/${fullWorkerConcatenation.scriptFilename}`],
  sourcemap: 'external',
  sourcesContent: DEBUG,
  minify: MINIFY,
  keepNames: true,
  outfile: "build/lib/worker-main.polyfilled.min.js"
});

const minifiedWorkerConcatenation = {
  script: fs.readFileSync('build/lib/worker-main.polyfilled.min.js'),
  sourcemapJSON: convertSourcemap.fromJSON(fs.readFileSync('build/lib/worker-main.polyfilled.min.js.map')).toObject(),
}

console.log();
console.log("Pass 4:  Wrapping + generating final output");

// Now, to build the wrapper...
let wrapper = `
// Autogenerated code.  Do not modify!
// --START:LMLayerWorkerCode--

export var LMLayerWorkerCode = "${encodeURIComponent(minifiedWorkerConcatenation.script)}"

export var LMLayerWorkerSourcemapComment = "//# sourceMappingURL=data:application/json;charset=utf-8;base64,${convertSourcemap.fromJSON(JSON.stringify(minifiedWorkerConcatenation.sourcemapJSON, null, 2)).toBase64()}";

// --END:LMLayerWorkerCode
`;

fs.writeFileSync('build/lib/worker-main.wrapped-for-bundle.js', wrapper);

// // For debugging, or if permanently loading from a file...

// // First one may need work - old link needs to be killed in favor of the second, most likely.
// // Then again, old link exists in the encoded version... and it's bypassed in favor of the true sourcemaps!
// fs.writeFileSync('build/lib/worker-main.bundled.js', fullWorkerConcatenation.script + '\n' + "//# sourceMappingURL=worker-main.bundled.js.map");
// fs.writeFileSync('build/lib/worker-main.bundled.js.map', JSON.stringify(fullWorkerConcatenation.sourcemapJSON, null, 2));

// // TS compilation will generally look for attached typing, and it's easy enough to provide.
// fs.writeFileSync('build/lib/worker-main.bundled.d.ts', `
// export var LMLayerWorkerCode: string;
// export var LMLayerWorkerSourcemapComment: string;
// `);