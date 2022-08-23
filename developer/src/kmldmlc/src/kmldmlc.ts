#!/usr/bin/env node
/**
 * kmldmlc - Keyman LDML Keyboard Compiler
 */

import * as fs from 'fs';
import * as path from 'path';
import * as program from 'commander';

import Compiler from './keyman/ldmlkeyboard/compiler';

let inputFilename: string;

const KEYMAN_VERSION = require("@keymanapp/keyman-version").KEYMAN_VERSION;

/* Arguments */
program
  .description('Compiles Keyman LDML keyboards')
  .version(KEYMAN_VERSION.VERSION_WITH_TAG)
  .arguments('<infile>')
  .action(infile => inputFilename = infile)
  .option('-o, --outFile <filename>', 'where to save the resulting .kmx file');

program.parse(process.argv);

// Deal with input arguments:
if (!inputFilename) {
  exitDueToUsageError('Must provide a LDML keyboard .xml source file.');
}

function exitDueToUsageError(message: string): never  {
  console.error(`${program._name}: ${message}`);
  console.error();
  program.outputHelp();
  return process.exit(64); // TODO: SysExits.EX_USAGE
}

class CompilerCallbacks {
  loadFile(baseFilename: string, filename:string): Buffer {
    // TODO: translate filename based on the baseFilename
    return fs.readFileSync(filename);
  }
  reportMessage(severity: number, message: string): void {
    console.log(message);
  }
}

function compileKeyboard(inputFilename: string): Uint8Array {
  const c = new CompilerCallbacks();
  const k = new Compiler(c);
  return k.compile(inputFilename);
}

// Compile:
let code = compileKeyboard(inputFilename);

// Output:

if(code) {
  const outFile = program.outFile ?? path.join(path.dirname(inputFilename), path.basename(inputFilename, '.xml') + '.kmx');
  fs.writeFileSync(outFile, code);
}
