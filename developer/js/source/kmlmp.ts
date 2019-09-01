#!/usr/bin/env node
/**
 * kmlmc - Keyman Lexical Model Package Compiler
 */

import * as program from 'commander';
import * as fs from 'fs';
import KmpCompiler from './package-compiler/kmp-compiler';

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
  .description('Compiles Keyman lexical model packages')
  .version(require('../package.json').version)
  .arguments('<infile>')
  .action(infile => inputFilename = infile)
  .option('-o, --outFile <filename>', 'where to save the resultant file');

program.parse(process.argv);

// Deal with input arguments:

if (!inputFilename) {
  exitDueToUsageError('Must provide a lexical model package source file.');
}

let outputFilename: string = program.outFile ? program.outFile : inputFilename.replace(/\.kps$/, ".kmp");

//
// Load .kps source data
//

let kpsString: string = fs.readFileSync(inputFilename, 'utf8');
let kmpCompiler = new KmpCompiler();
let kmpJsonData = kmpCompiler.transformKpsToKmpObject(kpsString);

//
// Build the .kmp package file
//

kmpCompiler.buildKmpFile(kmpJsonData, outputFilename);

function exitDueToUsageError(message: string): never  {
  console.error(`${program._name}: ${message}`);
  console.error();
  program.outputHelp();
  return process.exit(SysExits.EX_USAGE);
}
