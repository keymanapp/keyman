import SourcemapRemapper from "@keymanapp/sourcemap-path-remapper"
import convertSourceMap from 'convert-source-map'; // Transforms sourcemaps among various common formats.
                                                   // Base64, stringified-JSON, end-of-file comment...
import fs from 'fs';

function displayHelp() {
  console.log("KeymanWeb's sourcemap-cleansing tool.  This tool is designed to produce clean filepaths");
  console.log("in the sourcemaps for ease of reference in browser and Sentry, copying the clean paths TS");
  console.log("is able to provide, whereas Closure's minification is not.");
  console.log("");
  console.log("Usage:  node sourcemap-root <source.js> <source.map> [-c|--clean] [-s|--sourceRoot <path>]");
  console.log("        -c|--clean Runs source-filepath cleaning operations.");
  console.log("        -i|--inline Inlines <source.map> as a comment within <source.js>.");
  console.log("        -s|--sourceRoot <path> Sets the input sourcemap's 'sourceRoot' to <path>.");
  console.log("");
  console.log("        <source.js> is ignored unless the --inline option is specified.");
}

// By default, any node-based process has two command-line args:
// [0] - node installation root
// [1] - path to the script being run.

// Basic help-request check
if(process.argv.length < 4) {  // [2] and [3] - <source.map> and <dest.map> respectively.
  displayHelp();
  process.exit(1);
} else {
  switch(process.argv[2]) {
    case '--help':
    case '-h':
    case '-?':
      displayHelp();
      process.exit();
    default:
      // program continues as normal.
  }
}

function assert_is_map(filename: string) {
  if(filename.lastIndexOf(".map") != (filename.length - 4)) {
    console.error("Error:  sourcemap .map file was not provided as first argument.");
    console.log();
    displayHelp();
    process.exit(1);
  }
}

// Verify the second parameter is a sourcemap filepath.
let scriptFile = process.argv[2];
let mapFile = process.argv[3];

assert_is_map(mapFile);

// Now to process any additional flags.
var sourceRoot = "";
let shouldClean = false;
let shouldInline = false;

// Starting at [5] - optional command-line arguments
for(let procArgIndex = 4; procArgIndex < process.argv.length; procArgIndex++) {
  let flag = process.argv[procArgIndex];

  if(flag.indexOf('-') != 0) {
    console.error("Error:  Unexpected command-line argument provided: \"" + flag + "\" is not an argument flag")
    process.exit(1);
  }

  switch(flag) {
    case "-c":
    case "--clean":
      shouldClean = true;
      break;
    case "-i":
    case "--inline":
      shouldInline = true;
      break;
    case "-s":
    case "--sourceRoot":
      procArgIndex++;
      sourceRoot = process.argv[procArgIndex];
      break;
    default:
      console.warn("Unrecognized flag found in argument list: \"" + flag + "\"");
  }
}

let srcMap = SourcemapRemapper.fromFile(mapFile);

if(shouldClean) {
  // Keeps "@keymanapp/keyman" as sourceRoot, but prepends all `sources` entries with
  // the remainder / suffix of sourceRoot and normalizes the paths.
  srcMap.normalizeWithSourceRoot('@keymanapp/keyman/'.length);
}

if(sourceRoot) {
  srcMap.sourceRoot = sourceRoot;
}

srcMap.toFile(mapFile);

if(shouldInline) {
  const mapToInline = SourcemapRemapper.fromFile(mapFile).sourceMap;
  const inlineableComment = convertSourceMap.fromObject(mapToInline).toComment();

  fs.appendFileSync(scriptFile, `\n${inlineableComment}`);
}
