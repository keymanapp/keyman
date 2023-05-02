import fs from 'fs';

import SourceMapRemapper from '@keymanapp/sourcemap-path-remapper';

import SourceMapCombiner from 'combine-source-map';
import convertSourceMap from 'convert-source-map'; // Transforms sourcemaps among various common formats.
                                                   // Base64, stringified-JSON, end-of-file comment...

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

let fullWorkerConcatenation = concatScriptsAndSourcemaps(sourceFileSet, "worker-main.polyfilled.js", separatorFile);

// New stage:  cleaning the sourcemaps
console.log();

// Because we're compiling the main project based on its TS build outputs, and our cross-module references
// also link to build outputs, we need to clean up the sourcemap-paths.  Also, the individual module sourcemaps'
// paths result in unwanted extra pathing that needs to be cleaned up (models/models, correction/correction, etc)
console.log("Pass 2:  cleaning sourcemap source paths");

let remappingState = SourceMapRemapper
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
fullWorkerConcatenation.sourcemapJSON = remappingState.sourceMap;

// End "cleaning the sourcemaps"

console.log();
console.log("Pass 3:  Output intermediate state and perform minification");

// IMPORTANT: Remove file-end sourcemap ref comment and replace it!
fullWorkerConcatenation.script = fullWorkerConcatenation.script.substring(0, fullWorkerConcatenation.script.lastIndexOf('//# sourceMappingURL'));
fullWorkerConcatenation.script += `//# sourceMappingURL=${fullWorkerConcatenation.scriptFilename}.map`;

fs.writeFileSync(`build/lib/${fullWorkerConcatenation.scriptFilename}`, fullWorkerConcatenation.script);
fs.writeFileSync(`build/lib/${fullWorkerConcatenation.scriptFilename}.map`, convertSourceMap.fromObject(fullWorkerConcatenation.sourcemapJSON).toJSON());