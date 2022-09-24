#!/usr/bin/env node
/**
 * kmlmi - Keyman Lexical Model model_info Compiler
 */

import * as fs from 'fs';
import * as path from 'path';
import { Command } from 'commander';
import KmpCompiler from '@keymanapp/kmc-package';
import { ModelInfoOptions as ModelInfoOptions, writeMergedModelMetadataFile } from '@keymanapp/kmc-model-info';
import { SysExits } from './util/sysexits';
import KEYMAN_VERSION from "@keymanapp/keyman-version/keyman-version.mjs";

let inputFilename: string;
const program = new Command();

/* Arguments */
program
  .description('Merges Keyman lexical model model_info files. Intended for use within the keymanapp/lexical-models repository.')
  .version(KEYMAN_VERSION.VERSION_WITH_TAG)
  .arguments('<infile>')
  .action(infile => inputFilename = infile)
  .option('-o, --outFile <filename>', 'where to save the resultant file')
  .option('-m, --model <model_id>', 'model id, defaults to basename of input file sans .model_info extension')
  .option('-s, --source <source>', 'path to source of model, relative to lexical-models repo root')
  .option('--kpsFilename <kpsFilename>', 'path to .model.kps file, defaults to source/<model_id>.model.kps')
  .option('--kmpFilename <kmpFilename>', 'path to .model.kmp file, defaults to build/<model_id>.model.kmp')
  .option('--jsFilename <jsFilename>', 'path to .model.js file, defaults to build/<model_id>.model.js');

program.parse(process.argv);

// Deal with input arguments:

if (!inputFilename) {
  exitDueToUsageError('Must provide a lexical model .model_info source file.');
}

let model_id: string = program.model ? program.model : path.basename(inputFilename).replace(/\.model_info$/, "");
let outputFilename: string = program.outFile ? program.outFile : path.join(path.dirname(inputFilename), 'build', path.basename(inputFilename));
let kpsFilename = program.kpsFilename ? program.kpsFilename : path.join(path.dirname(inputFilename), 'source', path.basename(inputFilename).replace(/\.model_info$/, '.model.kps'));
let kmpFilename = program.kmpFilename ? program.kmpFilename : path.join(path.dirname(inputFilename), 'build', path.basename(inputFilename).replace(/\.model_info$/, '.model.kmp'));
let jsFilename = program.jsFilename ? program.jsFilename : path.join(path.dirname(inputFilename), 'build', path.basename(inputFilename).replace(/\.model_info$/, '.model.js'));

//
// Load .kps source data
//

let kpsString: string = fs.readFileSync(kpsFilename, 'utf8');
let kmpCompiler = new KmpCompiler();
let kmpJsonData = kmpCompiler.transformKpsToKmpObject(kpsFilename, kpsString);

//
// Write out the merged .model_info file
//

let modelInfoOptions: ModelInfoOptions = {
  model_id: model_id,
  kmpJsonData: kmpJsonData,
  sourcePath: program.source,
  modelFileName: jsFilename,
  kmpFileName: kmpFilename
};

try {
  writeMergedModelMetadataFile(
    inputFilename,
    outputFilename,
    modelInfoOptions);
} catch(e) {
  console.error(e);
  process.exit(SysExits.EX_DATAERR);
}

function exitDueToUsageError(message: string): never  {
  console.error(`${program._name}: ${message}`);
  console.error();
  program.outputHelp();
  return process.exit(SysExits.EX_USAGE);
}
