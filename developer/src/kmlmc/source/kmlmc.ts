#!/usr/bin/env node
/**
 * kmlmc - Keyman Lexical Model Compiler
 */
/// <reference path="lexical-model-compiler/lexical-model.ts" />

import * as fs from 'fs';
import * as program from 'commander';

import { compileModel } from './util/util';
import { SysExits } from './util/sysexits';

let inputFilename: string;

const KEYMAN_VERSION = require("@keymanapp/keyman-version").KEYMAN_VERSION;

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

// Compile:
let code = compileModel(inputFilename);

// Output:
if (program.outFile) {
  fs.writeFileSync(program.outFile, code, 'utf8');
} else {
  console.log(code);
}

function exitDueToUsageError(message: string): never  {
  console.error(`${program._name}: ${message}`);
  console.error();
  program.outputHelp();
  return process.exit(SysExits.EX_USAGE);
}