#!/usr/bin/env node
/**
 * kmlmc - Keyman Lexical Model Compiler
 */
/// <reference path="lexical-model-compiler/lexical-model.ts" />

import * as fs from 'fs';
import * as program from 'commander';

import LexicalModelCompiler from "./";
import { loadFromFilename } from './loadFromFilename';

/**
 * Exit codes defined in <sysexits.h>:
 * https://www.freebsd.org/cgi/man.cgi?query=sysexits&apropos=0&sektion=0&manpath=FreeBSD+4.3-RELEASE&format=html
 */
export const enum SysExits {
  EX_USAGE = 64,
  EX_DATAERR = 65,
};

let inputFilename: string;

/* Arguments */
program
  .description('Compiles Keyman lexical models')
  .version(require('../package.json').version)
  .arguments('<infile>')
  .action(infile => inputFilename = infile)
  .option('-o, --outFile <filename>', 'where to save the resultant file');

program.parse(process.argv);

// Deal with input arguments:
if (!inputFilename) {
  exitDueToUsageError('Must provide a lexical model source file.');
}

// Compile:
let o = loadFromFilename(inputFilename);
let code = (new LexicalModelCompiler).generateLexicalModelCode('<unknown>', o, '.');

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