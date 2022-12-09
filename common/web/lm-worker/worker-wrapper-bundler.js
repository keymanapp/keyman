import fs from 'fs';

import SourcemapCombiner from 'combine-source-map';
import convertSourcemap from 'convert-source-map'; // Transforms sourcemaps among various common formats.
                                                   // Base64, stringified-JSON, end-of-file comment...

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

let separatorFile = {
  source: `

  `,
  sourceFile: '<inline>'
}

let sourceFileSet = [
  // Needed for Android / Chromium browser pre-45.
  loadPolyfill('src/polyfills/array.fill.js', 'polyfills/array.fill.js'),
  // Needed for Android / Chromium browser pre-45.
  loadPolyfill('src/polyfills/array.findIndex.js', 'polyfills/array.findIndex.js'),
  // Needed for Android / Chromium browser pre-45.
  loadPolyfill('src/polyfills/array.from.js', 'polyfills/array.from.js'),
  // Needed for Android / Chromium browser pre-47.
  loadPolyfill('src/polyfills/array.includes.js', 'polyfills/array.includes.js'),
  // For Object.values, for iteration over object-based associate arrays.
  // Needed for Android / Chromium browser pre-54.
  loadPolyfill('src/polyfills/object.values.js', 'polyfills/object.values.js'),
  // Needed to support Symbol.iterator, as used by the correction algorithm.
  // Needed for Android / Chromium browser pre-43.
  loadPolyfill('src/polyfills/symbol-es6.min.js', 'polyfills/symbol-es6.min.js'),
  loadCompiledModuleFilePair('build/lib/worker-main.mjs', 'worker-main.mjs'),
];

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
let fullWorkerConcatenation = concatScriptsAndSourcemaps(sourceFileSet, "worker-main.polyfilled.js", separatorFile);

// New stage:  cleaning the sourcemaps

console.log();

// Because we're compiling the main project based on its TS build outputs, and our cross-module references
// also link to build outputs, we need to clean up the sourcemap-paths.  Also, the individual module sourcemaps'
// paths result in unwanted extra pathing that needs to be cleaned up (models/models, correction/correction, etc)
console.log("Pass 2:  cleaning sourcemap source paths");
let sourcemapPathMap = [
  {from: 'polyfills/', to: '/common/web/lm-worker/src/polyfills/'},
  {from: 'models/templates/build/obj/', to :'common/models/templates/src/'},
  {from: 'models/wordbreakers/build/obj/default/default', to: 'common/models/wordbreakers/src/default/'},
  {from: 'models/wordbreakers/build/obj/', to: 'common/models/wordbreakers/src/'},
  {from: 'obj/models/models/', to: 'common/web/lm-worker/src/models/'},
  {from: 'obj/correction/correction/', to: 'common/web/lm-worker/src/correction/'},
  {from: 'obj/', to: 'common/web/lm-worker/src/'},
  {from: 'utils/src/', to: 'common/web/utils/src/'}
];

let mapSources = fullWorkerConcatenation.sourcemapJSON.sources;
let unmapped = [];
for(let i = 0; i < mapSources.length; i++) {
  let matched = false;
  for(let map of sourcemapPathMap) {
    if(mapSources[i].includes(map.from)) {
      let originalPath = mapSources[i];
      mapSources[i] = mapSources[i].replace(map.from, map.to);
      console.log(`- ${originalPath} -> ${mapSources[i]}`);
      matched = true;
      break;
    }
  }

  if(!matched) {
    unmapped.push(mapSources[i]);
  }
}

if(unmapped.length > 0) {
  console.log("Not mapped:");

  for(let path of unmapped) {
    console.log(`- ${path}`);
  }
}

console.log();
let sourceRoot = "/@keymanapp/keyman";
console.log(`Setting sourceRoot: ${sourceRoot}`)
fullWorkerConcatenation.sourcemapJSON.sourceRoot = sourceRoot;

// End "cleaning the sourcemaps"

// NOTE:  At this stage, if desired, the JSON form of the sourcemap may be cleaned, source paths altered, etc
// before proceeding!

// Now, to build the wrapper...
let wrapper = `
// Autogenerated code.  Do not modify!
// --START:LMLayerWorkerCode--

export var LMLayerWorkerCode = "${encodeURIComponent(fullWorkerConcatenation.script)}"

export var LMLayerWorkerSourcemapComment = "//# sourceMappingURL=data:application/json;charset=utf-8;base64,${convertSourcemap.fromJSON(JSON.stringify(fullWorkerConcatenation.sourcemapJSON, null, 2)).toBase64()}";

// --END:LMLayerWorkerCode
`;

console.log();
console.log("Pass 3:  Wrapping + generating final output");
fs.writeFileSync('build/lib/worker-main.wrapped-for-bundle.js', wrapper);

// For debugging, or if permanently loading from a file...

// First one may need work - old link needs to be killed in favor of the second, most likely.
// Then again, old link exists in the encoded version... and it's bypassed in favor of the true sourcemaps!
fs.writeFileSync('build/lib/worker-main.bundled.js', fullWorkerConcatenation.script + '\n' + "//# sourceMappingURL=worker-main.bundled.js.map");
fs.writeFileSync('build/lib/worker-main.bundled.js.map', JSON.stringify(fullWorkerConcatenation.sourcemapJSON, null, 2));

// Will have sourcemap link for original file... then the loaded form that we build earlier on that gets concat'd.
// THEN the actual, final one.

// But, a problem:  the sourcemap is, by default, a comment... and comments don't pass through Function.toString().
// SOLVED!  (Separate var for the sourcemap, appended to the 'unwrapped' worker.)  And sourcemaps are showing up!







// -------------- Development notes of struggles I ran into & related solutions ---------------

// Once I got sourcemaps to show up, they were misaligned.
// Sourcemaps for the wrapped worker don't get processed during es-bundling or TSC re-compilation after content prepending.
// At the LM-Layer unit-testing level, it's mostly due to whitespaces & comments in the polyfills as of this point;
// those don't really pass through a Function.toString(), after all.
//
// Could _easily_ get worse with es-bundling optimizations for the 'final' level, which may further manipulate the source.
//
// Temp-solved!  ("Wrapped" via encodeURIComponent as a pure, encoded string - not as minifiable JS.)

// After that... there was a "fudge factor" to discern... but WE'RE OPERATIONAL, BABY!  A WORKING WORKER SOURCEMAP!