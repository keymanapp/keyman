#!/usr/bin/env node
import * as fs from 'fs';
import * as rd from 'readline';
import * as program from 'commander';

let inputFilename: string = "";
let outputFilename: string = "";

program
  .description(
`Will convert the input file which is a hex dump to the output file. Hex dump can contain
blank lines, offsets, commands and comments. File writing will start at zero, padding is
not currently supported.

Format:

  # This is a comment
  block(name)                     # each part of file must be defined as a block
  11 22 33 44 55 aa bb            # inserts hex bytes
  offset(block)                   # inserts 4 byte offset of block from BOF
  sizeof(block[,divisor])         # inserts 4 byte size of block, optionally divided by divisor
  diff(block1,block2)             # inserts 4 byte offset diff between start of block1 and block2
  index(block1,block2[,divisor])  # inserts 4 byte index diff between block1 and block2,
                                  # optionally divided by divisor
  # block names are required, but if not referenced can be reused (e.g. block(x))
`
  )
  .arguments('<infile>')
  .arguments('<outfile>')
  .action((infile:any, outfile:any) => { inputFilename = infile; outputFilename = outfile; });

program.parse(process.argv);

if(!inputFilename || !outputFilename) {
  exitDueToUsageError('Must provide an input and output filename.');
}

function exitDueToUsageError(message: string): never  {
  console.error(`${program._name}: ${message}`);
  console.error();
  program.outputHelp();
  return process.exit(64); // SysExits.EX_USAGE
}

import hextobin from './index';

hextobin(inputFilename, outputFilename, {silent: false});
