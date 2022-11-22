#!/usr/bin/env node
/**
 * kmlmp - Keyman Lexical Model Package Compiler
 */

import * as program from 'commander';
import * as fs from 'fs';
import KmpCompiler from './package-compiler/kmp-compiler';
import { SysExits } from './util/sysexits';
const KEYMAN_VERSION = require("@keymanapp/keyman-version").KEYMAN_VERSION;

let inputFilename: string;

/* Arguments */
program
  .description('Compiles Keyman lexical model packages')
  .version(KEYMAN_VERSION.VERSION_WITH_TAG)
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
let kmpJsonData = kmpCompiler.transformKpsToKmpObject(kpsString, inputFilename);

//
// Build the .kmp package file
//

let promise = kmpCompiler.buildKmpFile(kmpJsonData);
promise.then(data => {
  fs.writeFileSync(outputFilename, data, 'binary');
}).catch(error => {
  // Consumer decides how to handle errors
  console.error('Failed to write kmp file');
});

function exitDueToUsageError(message: string): never  {
  console.error(`${program._name}: ${message}`);
  console.error();
  program.outputHelp();
  return process.exit(SysExits.EX_USAGE);
}
