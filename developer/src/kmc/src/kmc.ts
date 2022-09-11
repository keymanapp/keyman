#!/usr/bin/env node
/**
 * kmc - Keyman LDML Keyboard Compiler
 */

import * as fs from 'fs';
import * as path from 'path';
import * as program from 'commander';

import Compiler from './keyman/compiler/compiler';
import KMXBuilder from './keyman/kmx/kmx-builder';
import { CompilerMessages } from './keyman/compiler/messages';
import { CompilerEvent } from './keyman/compiler/callbacks';

let inputFilename: string;

const KEYMAN_VERSION = require("@keymanapp/keyman-version").KEYMAN_VERSION;

/* Arguments */
program
  .description('Compiles Keyman LDML keyboards')
  .version(KEYMAN_VERSION.VERSION_WITH_TAG)
  .arguments('<infile>')
  .action((infile:any) => inputFilename = infile)
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
  reportMessage(event: CompilerEvent): void {
    console.log(CompilerMessages.severityName(event.code) + ' ' + event.code.toString(16) + ': ' + event.message);
  }
  loadLdmlKeyboardSchema(): Buffer {
    return fs.readFileSync(path.join(__dirname, 'ldml-keyboard.schema.json'));
  }
}

function compileKeyboard(inputFilename: string): Uint8Array {
  const c = new CompilerCallbacks();
  const k = new Compiler(c);
  let source = k.load(inputFilename);
  if(!source) {
    return null;
  }
  if(!k.validate(source)) {
    return null;
  }
  let kmx = k.compile(source);
  if(!kmx) {
    return null;
  }

  // Use the builder to generate the binary output file
  let builder = new KMXBuilder(kmx, true);
  return builder.compile();
}

// Compile:
let code = compileKeyboard(inputFilename);

// Output:

if(code) {
  const outFile = program.outFile ?? path.join(path.dirname(inputFilename), path.basename(inputFilename, '.xml') + '.kmx');
  console.log(`Writing compiled keyboard to ${outFile}`);
  fs.writeFileSync(outFile, code);
}
