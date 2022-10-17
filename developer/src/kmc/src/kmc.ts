#!/usr/bin/env node
/**
 * kmc - Keyman Next Generation Compiler
 */

import * as fs from 'fs';
import * as path from 'path';

import { Command } from 'commander';
import * as kmc from '@keymanapp/kmc-keyboard';
import KEYMAN_VERSION from "@keymanapp/keyman-version/keyman-version.mjs";
import { KvkFileWriter } from '@keymanapp/common-types';

let inputFilename: string;

const program = new Command();
/* Arguments */
program
  .description('Compiles Keyman LDML keyboards')
  .version(KEYMAN_VERSION.VERSION_WITH_TAG)
  .arguments('<infile>')
  .action((infile:any) => inputFilename = infile)
  .option('-d, --debug', 'Include debug information in output')
  .option('--no-compiler-version', 'Exclude compiler version metadata from output')
  .option('-o, --out-file <filename>', 'where to save the resulting .kmx file');

program.parse(process.argv);

// Deal with input arguments:
if (!inputFilename) {
  exitDueToUsageError('Must provide a LDML keyboard .xml source file.');
}

function exitDueToUsageError(message: string): never  {
  console.error(`${program._name}: ${message}`);
  console.error();
  program.outputHelp();
  return process.exit(64); // TODO: SysExits.EX_USAGE
}

class CompilerCallbacks {
  loadFile(baseFilename: string, filename:string): Buffer {
    // TODO: translate filename based on the baseFilename
    return fs.readFileSync(filename);
  }
  reportMessage(event: kmc.CompilerEvent): void {
    console.log(kmc.CompilerMessages.severityName(event.code) + ' ' + event.code.toString(16) + ': ' + event.message);
  }
  loadLdmlKeyboardSchema(): Buffer {
    let schemaPath = new URL('ldml-keyboard.schema.json', import.meta.url);
    return fs.readFileSync(schemaPath);
  }
  loadKvksJsonSchema(): Buffer {
    let schemaPath = new URL('kvks.schema.json', import.meta.url);
    return fs.readFileSync(schemaPath);
  }
}

function compileKeyboard(inputFilename: string, options: kmc.CompilerOptions): [Uint8Array,Uint8Array] {
  const c = new CompilerCallbacks();
  const k = new kmc.Compiler(c, options);
  let source = k.load(inputFilename);
  if(!source) {
    return [null, null];
  }
  if(!k.validate(source)) {
    return [null, null];
  }
  let kmx = k.compile(source);
  if(!kmx) {
    return [null, null];
  }

  // In order for the KMX file to be loaded by non-KMXPlus components, it is helpful
  // to duplicate some of the metadata
  kmc.KMXPlusMetadataCompiler.addKmxMetadata(kmx.kmxplus, kmx.keyboard, options);

  // Use the builder to generate the binary output file
  const builder = new kmc.KMXBuilder(kmx, options.debug);
  const kmx_binary = builder.compile();

  const vkcompiler = new kmc.VisualKeyboardCompiler();
  const vk = vkcompiler.compile(source);
  const writer = new KvkFileWriter();
  const kvk_binary = writer.write(vk);

  return [kmx_binary, kvk_binary];
}

let options: kmc.CompilerOptions = {
  debug: program.debug ?? false,
  addCompilerVersion: program.compilerVersion ?? true
}

// TODO-LDML: consider hardware vs touch -- touch-only layout will not have a .kvk
// Compile:
let [kmx,kvk] = compileKeyboard(inputFilename, options);

// Output:

if(kmx && kvk) {
  const fileBaseName = program.outFile ?? inputFilename;
  const outFileBase = path.basename(fileBaseName, path.extname(fileBaseName));
  const outFileDir = path.dirname(fileBaseName);

  const outFileKmx = path.join(outFileDir, outFileBase + '.kmx');
  console.log(`Writing compiled keyboard to ${outFileKmx}`);
  fs.writeFileSync(outFileKmx, kmx);

  const outFileKvk = path.join(outFileDir, outFileBase + '.kvk');
  console.log(`Writing compiled visual keyboard to ${outFileKvk}`);
  fs.writeFileSync(outFileKvk, kvk);
} else {
  console.error(`An error occurred compiling ${inputFilename}`);
  process.exit(1);
}
