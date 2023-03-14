#!/usr/bin/env node
/**
 * kmlmc - Keyman Lexical Model Compiler
 */

import * as fs from 'fs';
import { Command } from 'commander';
import { compileModel } from '@keymanapp/kmc-model';
import { SysExits } from './util/sysexits';
import KEYMAN_VERSION from "@keymanapp/keyman-version";

let inputFilename: string;
const program = new Command();

/* Arguments */
program
  .description('Compiles Keyman lexical models')
  .version(KEYMAN_VERSION.VERSION_WITH_TAG)
  .arguments('<infile>')
  .action(infile => inputFilename = infile)
  .option('-o, --outFile <filename>', 'where to save the resultant file');

program.parse(process.argv);

// Deal with input arguments:
if (!inputFilename) {
  exitDueToUsageError('Must provide a lexical model source file.');
}

let code = null;
// Compile:
try {
  code = compileModel(inputFilename);
} catch(e) {
  console.error(e);
  process.exit(SysExits.EX_DATAERR);
}

if(!code) {
  console.error('Compilation failed.')
  process.exit(SysExits.EX_DATAERR);
}

// Output:
if (program.opts().outFile) {
  fs.writeFileSync(program.opts().outFile, code, 'utf8');
} else {
  console.log(code);
}

function exitDueToUsageError(message: string): never  {
  console.error(`${program.name()}: ${message}`);
  console.error();
  program.outputHelp();
  return process.exit(SysExits.EX_USAGE);
}