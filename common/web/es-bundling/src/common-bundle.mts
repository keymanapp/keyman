import fs from 'fs';
import esbuild from 'esbuild';
import { esmConfiguration, forES6, iifeConfiguration } from './configuration.mjs';
import { prepareTslibTreeshaking } from './tslibTreeshaking.mjs';

let CHARSET = 'ascii';
let FORMAT = 'iife';
let MINIFY = false;

let sourceFromArgs;
let destFromArgs;
let profilePath;
let sourceRoot;
let platform;

let jsVersionTarget='es6';

function doHelp(errCode?: number) {
  console.log(`
Summary:
  Uses esbuild to generate bundled-JS according to common, repo-wide KeymanWeb-oriented settings.

Usage:
  common-bundle.mjs <input-file> --out <out-file> [options...]

Parameters:
  <input-file>:         Fully-bundled and compiled JS file to be wrapped.
  <out-file>:           Specifies the destination path for the wrapped output.

Options:
  --help                Shows this script's documentation
  --charset=<charset>   Sets the charset type for esbuild to emit.  Defaults to 'ascii'
                        but may also be 'utf8'.
  --format=<format>     Sets the format type to use for the generated bundle.  Should be
                        'iife' or 'esm'.

                        If not specified, 'iife' will be used.
  --minify              Enables minification.
  --platform=<platform> Sets the 'platform' property of the esbuild config accordingly.
  --profile=<out-file>  Generates an associated filesize profile at the specified path.
  --sourceRoot=<path>   Sets the sourceRoot for generated source maps
  --target=<target>     Sets the JavaScript / ECMAScript version to target for the bundle.
` );
  process.exit(errCode || 0);
}

if(process.argv.length > 2) {
  for(let i = 2; i < process.argv.length; i++) {
    const arg = process.argv[i];

    switch(arg) {
      case '--help':
        doHelp();
        break;
      case '--charset':
        let charsetOption = process.argv[++i];
        switch(charsetOption) {
          case 'ascii':
          case 'utf8':
            CHARSET = charsetOption;
            break;
          default:
            console.error(`Invalid bundling format specified: ${charsetOption}.  Must be 'ascii' or 'utf8'.`);
            doHelp(1);
            break;
        }
        break;
      case '--format':  // bc TS uses this exact flag.  esbuild... uses sourcemap (in the JS config)
        let formatOption = process.argv[++i];
        switch(formatOption) {
          case 'iife':
          case 'esm':
            FORMAT = formatOption;
            break;
          default:
            console.error(`Invalid bundling format specified: ${formatOption}.  Must be 'iife' or 'esm'.`);
            doHelp(1);
            break;
        }
        break;
      case '--minify':
        MINIFY = true;
        break;
      case '--out':
        destFromArgs = process.argv[++i];
        break;
      case '--platform':
        platform = process.argv[++i];
        break;
      case '--profile':
        profilePath = process.argv[++i];
        break;
      case '--sourceRoot':
        sourceRoot = process.argv[++i];
        break;
      case '--target':
        jsVersionTarget = process.argv[++i];
        break;
      default:
        if(!sourceFromArgs) {
          sourceFromArgs = arg;
        } else {
          doHelp(1);
        }
    }
  }
} else {
  // Not enough args; display help + abort.
  doHelp(1);
}

function fileSpecError(type: 'input' | 'output', file?: string) {
  console.error(`${file ? 'Invalid' : 'No'} ${type} file ${file ? `(${file})`: ''} has been specified; aborting.`);
  console.log();
  doHelp(1);
}

function checkFileSpec(file: string, type: 'input' | 'output') {
  if(!file) {
    fileSpecError(type);
  } else {
    const ext = file.substring(file.lastIndexOf('.'));
    switch(ext) {
      case '.js':
      case '.mjs':
      case '.ts':
      case '.mts':
        break;
      default:
        fileSpecError(type, file);
    }
  }
}

checkFileSpec(sourceFromArgs, 'input');
checkFileSpec(destFromArgs, 'output');

const sourceFile = sourceFromArgs;
const destFile = destFromArgs;

// And now to start the actual bundling config + operation.
const baseConfig = FORMAT == 'iife' ? iifeConfiguration : esmConfiguration;

const config: esbuild.BuildOptions = {
  ...jsVersionTarget == 'es6' ? forES6(baseConfig) : baseConfig,
  entryPoints: [sourceFile],
  charset: CHARSET as 'ascii' | 'utf8',
  outfile: destFile,
  minify: MINIFY,
  metafile: !!profilePath,
  sourceRoot: sourceRoot, // may be undefined - is fine if so.
  platform: platform as esbuild.Platform || "browser"
};

await prepareTslibTreeshaking(config);
const results = await esbuild.build(config);

if(results.metafile) {
  let filesizeProfile = await esbuild.analyzeMetafile(results.metafile, { verbose: true });
  fs.writeFileSync(profilePath, filesizeProfile);
}
