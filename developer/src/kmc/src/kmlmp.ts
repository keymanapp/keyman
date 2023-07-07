#!/usr/bin/env node
/**
 * kmlmp - Keyman Lexical Model Package Compiler
 */

// Note: this is a deprecated package and will be removed in Keyman 18.0

import * as fs from 'fs';
import { Command } from 'commander';
import { PackageValidation, KmpCompiler } from '@keymanapp/kmc-package';
import { SysExits } from './util/sysexits.js';
import KEYMAN_VERSION from "@keymanapp/keyman-version";
import { NodeCompilerCallbacks } from './messages/NodeCompilerCallbacks.js';

let inputFilename: string;
const program = new Command();

/* Arguments */
program
  .description('Compiles Keyman lexical model packages\nDeprecated: use <kmc build> instead; will be removed in v18')
  .version(KEYMAN_VERSION.VERSION_WITH_TAG)
  .arguments('<infile>')
  .action(infile => inputFilename = infile)
  .option('-o, --outFile <filename>', 'where to save the resultant file');

program.parse(process.argv);

// Deal with input arguments:

if (!inputFilename) {
  exitDueToUsageError('Must provide a lexical model package source file.');
}

let outputFilename: string = program.opts().outFile ? program.opts().outFile : inputFilename.replace(/\.kps$/, ".kmp");

//
// Load .kps source data
//

const callbacks = new NodeCompilerCallbacks({logLevel: 'info'});
let kmpCompiler = new KmpCompiler(callbacks);
let kmpJsonData = kmpCompiler.transformKpsToKmpObject(inputFilename);
if(!kmpJsonData) {
  process.exit(1);
}

//
// Validate the package file
//

const validation = new PackageValidation(callbacks);
if(!validation.validate(inputFilename, kmpJsonData)) {
  process.exit(1);
}

//
// Build the .kmp package file
//

let promise = kmpCompiler.buildKmpFile(inputFilename, kmpJsonData);
promise.then(data => {
  fs.writeFileSync(outputFilename, data, 'binary');
}).catch(error => {
  // Consumer decides how to handle errors
  console.error('Failed to write kmp file');
});

function exitDueToUsageError(message: string): never  {
  console.error(`${program.name()}: ${message}`);
  console.error();
  program.outputHelp();
  return process.exit(SysExits.EX_USAGE);
}
