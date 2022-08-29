#!/usr/bin/env node
import * as fs from 'fs';
import * as rd from 'readline';
import * as program from 'commander';

let inputFilename: string = "";
let outputFilename: string = "";

program
  .description(
`Will convert the input file which is a hex dump to the output file. Hex dump can contain
blank lines, offsets and comments. File writing will start at zero if no offset given;
any blank spaces in the file will be filled with nul bytes.

0x1234: 11 22 33 44 55 aa bb     # This is a comment
# offset(0xhex|dec): hex bytes            # comment`
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

hextobin(inputFilename, outputFilename, {silent: true});
