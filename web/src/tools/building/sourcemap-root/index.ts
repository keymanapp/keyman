import SourcemapRemapper from "@keymanapp/sourcemap-path-remapper"

function displayHelp() {
  console.log("KeymanWeb's sourcemap-cleansing tool.  This tool is designed to produce clean filepaths");
  console.log("in the sourcemaps for ease of reference in browser and Sentry, copying the clean paths TS");
  console.log("is able to provide, whereas Closure's minification is not.");
  console.log("");
  console.log("Usage:  node sourcemap-root <source.map> <dest.map> [-c|--clean] [-s|--sourceRoot <path>]");
  console.log("        -c|--clean Runs source-filepath cleaning operations.");
  console.log("        -s|--sourceRoot <path> Sets the input sourcemap's 'sourceRoot' to <path>.");
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
// Verify the first parameter is a sourcemap filepath.
// let sourceFile = process.argv[2]; // Used to be necessary, but no longer is.
let destFile = process.argv[3];

assert_is_map(destFile);

// Now to process any additional flags.
var sourceRoot = "";
let shouldClean = false;

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
    case "-s":
    case "--sourceRoot":
      procArgIndex++;
      sourceRoot = process.argv[procArgIndex];
      break;
    default:
      console.warn("Unrecognized flag found in argument list: \"" + flag + "\"");
  }
}

let srcMap = SourcemapRemapper.fromFile(destFile);

if(shouldClean) {
  // Keeps "@keymanapp/keyman" as sourceRoot, but prepends all `sources` entries with
  // the remainder / suffix of sourceRoot and normalizes the paths.
  srcMap.normalizeWithSourceRoot('@keymanapp/keyman/'.length);
}

if(sourceRoot) {
  srcMap.sourceRoot = sourceRoot;
}

srcMap.toFile(destFile);