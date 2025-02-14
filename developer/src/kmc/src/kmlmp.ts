#!/usr/bin/env node
/**
 * kmlmp - Keyman Lexical Model Package Compiler
 */

// Note: this is a deprecated package and will be removed in Keyman 18.0

import { Command } from 'commander';
import { KmpCompiler } from '@keymanapp/kmc-package';
import { SysExits } from './util/sysexits.js';
import KEYMAN_VERSION from "@keymanapp/keyman-version";
import { NodeCompilerCallbacks } from './util/NodeCompilerCallbacks.js';

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
// Run the compiler
//

const callbacks = new NodeCompilerCallbacks({logLevel: 'info'});
let kmpCompiler = new KmpCompiler();
if(!await kmpCompiler.init(callbacks, null)) {
  process.exit(1);
}

let result = await kmpCompiler.run(inputFilename, outputFilename);
if(!result) {
  process.exit(1);
}

if(!await kmpCompiler.write(result.artifacts)) {
  console.error('Failed to write kmp file');
  process.exit(1);
}

function exitDueToUsageError(message: string): never  {
  console.error(`${program.name()}: ${message}`);
  console.error();
  program.outputHelp();
  return process.exit(SysExits.EX_USAGE);
}
