var fs = require("fs");
function displayHelp() {
    console.log("KeymanWeb's sourcemap-cleansing tool.  This tool is designed to produce clean filepaths");
    console.log("in the sourcemaps for ease of reference in browser and Sentry, copying the clean paths TS");
    console.log("is able to provide, whereas Closure's minification is not.");
    console.log("");
    console.log("Usage:  node sourcemap-root <source.map> <dest.map> [-s|--suffix <path>]");
    console.log("        -s|--suffix <path> Appends <path> to the input sourcemap's 'sourceRoot'.");
}
// By default, any node-based process has two command-line args:
// [0] - node installation root
// [1] - path to the script being run.
// Basic help-request check
if (process.argv.length < 4) { // [2] and [3] - <source.map> and <dest.map> respectively.
    displayHelp();
    process.exit(1);
}
else {
    switch (process.argv[2]) {
        case '--help':
        case '-h':
        case '-?':
            displayHelp();
            process.exit();
        default:
        // program continues as normal.
    }
}
function assert_is_map(filename) {
    if (filename.lastIndexOf(".map") != (filename.length - 4)) {
        console.error("Error:  sourcemap .map file was not provided as first argument.");
        console.log();
        displayHelp();
        process.exit(1);
    }
}
// Verify the first parameter is a sourcemap filepath.
var sourceFile = process.argv[2];
var destFile = process.argv[3];
assert_is_map(sourceFile);
assert_is_map(destFile);
// Now to process any additional flags.
var sourceRootSuffix = "";
// Starting at [5] - optional command-line arguments
for (var procArgIndex = 4; procArgIndex < process.argv.length; procArgIndex++) {
    var flag = process.argv[procArgIndex];
    if (flag.indexOf('-') != 0) {
        console.error("Error:  Unexpected command-line argument provided: \"" + flag + "\" is not an argument flag");
        process.exit(1);
    }
    switch (flag) {
        case "-s":
        case "--suffix":
            procArgIndex++;
            sourceRootSuffix = process.argv[procArgIndex];
            break;
        default:
            console.warn("Unrecognized flag found in argument list: \"" + flag + "\"");
    }
}
var sourceContent;
var destContent;
try {
    sourceContent = fs.readFileSync(sourceFile);
    destContent = fs.readFileSync(destFile);
}
catch (err) {
    console.error("Could not read specified input files");
    console.error("");
    console.error(err);
    process.exit(1);
}
var sourceJSON = JSON.parse(sourceContent);
var destJSON = JSON.parse(destContent);
// We now have the root JSON contents.  Time to go to work!  First, the easy parts.
destJSON["file"] = sourceJSON["file"];
destJSON["sourceRoot"] = sourceJSON["sourceRoot"] + sourceRootSuffix;
// The hard part:  mapping the source file arrays correctly.
// A raw copy of the arrays will not work properly.
var sourceSources = sourceJSON["sources"];
var destSources = destJSON["sources"];
var finalDestSources = [];
/**
 * Closure's minification always uses the TS paths as a suffix, prepending relative minification-path info to reuse them.
 * So, if we can find the TS path complete within a path in the minified sourcemap, it's a match.
 *
 * It's key that we preserve the original ordering of the source files from Closure's output sourcemaps, as is done here.
 * Otherwise, browsers will incorrectly match the inlined source to any given file, causing a fair bit of confusion.
 * This is why we iterate over the destination's defined array; this way ensures identical ordering for the finalized version.
 */
for (var _i = 0, destSources_1 = destSources; _i < destSources_1.length; _i++) {
    var destPath = destSources_1[_i];
    var matched = false;
    for (var _a = 0, sourceSources_1 = sourceSources; _a < sourceSources_1.length; _a++) {
        var sourcePath = sourceSources_1[_a];
        if (destPath.indexOf(sourcePath) != -1) {
            matched = true;
            finalDestSources.push(sourcePath);
            break;
        }
    }
    if (!matched) {
        finalDestSources.push(destPath);
    }
}
destJSON["sources"] = finalDestSources;
var destOutput = JSON.stringify(destJSON);
fs.writeFileSync(destFile, destOutput);
