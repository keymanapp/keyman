#!/usr/bin/env node
import { Command } from 'commander';
import hextobin from './index.js';

let inputFilename: string = "";
let outputFilename: string = "";

const program = new Command();
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
  diff(block1,block2[,divisor])  # inserts 4 byte offset diff between start of block1 and block2, ÷divisor (default: ÷1)
  index(block1,block2[,divisor])  # inserts 4 byte index diff between block1 and block2,
                                  # optionally divided by divisor
  # block names are required, but if not referenced can be reused (e.g. block(x))
  # Spaces after commas aren't allowed:  'sizeof(block,3)' not 'sizeof(block, 3)'
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

hextobin(inputFilename, outputFilename, { silent: false })
  .then((buffer) => {
    if (buffer) {
      process.exit(0); // OK
    } else {
      console.error(`${program._name}: Failed.`);
      process.exit(1); // no buffer - some err.
    }
  }, (err) => {
    console.error(err);
    console.error(`${program._name}: Failed.`);
    process.exit(1);
  });
