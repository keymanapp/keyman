#!/usr/bin/env node
/**
 * kmlmc - Keyman Lexical Model Compiler
 */

import { Command } from 'commander';
import { LexicalModelCompiler } from '@keymanapp/kmc-model';
import { SysExits } from './util/sysexits.js';
import KEYMAN_VERSION from "@keymanapp/keyman-version";
import { NodeCompilerCallbacks } from './util/NodeCompilerCallbacks.js';

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

const callbacks = new NodeCompilerCallbacks({logLevel: 'info'});

const compiler = new LexicalModelCompiler();
if(!await compiler.init(callbacks, null)) {
  console.error('Initialization failed.');
  process.exit(SysExits.EX_DATAERR);
}

let code = null;
// Compile:
try {
  code = await compiler.run(inputFilename, program.opts().outFile);
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
  compiler.write(code.artifacts);
} else {
  // TODO(lowpri): if writing to console then log messages should all be to stderr?
  const decoder = new TextDecoder();
  const text = decoder.decode(code.artifacts.js.data);
  console.log(text);
}

function exitDueToUsageError(message: string): never  {
  console.error(`${program.name()}: ${message}`);
  console.error();
  program.outputHelp();
  return process.exit(SysExits.EX_USAGE);
}