#!/usr/bin/env node
/**
 * kmc - Keyman Next Generation Compiler
 */

import * as fs from 'fs';
import * as path from 'path';

import { Command } from 'commander';
import * as kmc from '@keymanapp/kmc-keyboard';
import KEYMAN_VERSION from "@keymanapp/keyman-version/keyman-version.mjs";
import { KvkFileWriter, CompilerCallbacks, CompilerEvent, LDMLKeyboardTestDataXMLSourceFile } from '@keymanapp/common-types';

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
  .option('-o, --out-file <filename>', 'where to save the resulting .kmx file')
  .option('-T, --test-data <filename>', 'Convert keyboard test .xml to .json');

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

/**
 * Concrete implementation for CLI use
 */
class NodeCompilerCallbacks implements CompilerCallbacks {
  loadFile(baseFilename: string, filename: string | URL): Buffer {
    // TODO: translate filename based on the baseFilename
    try {
      return fs.readFileSync(filename);
    } catch(e) {
      if (e.code === 'ENOENT') {
        return null;
      } else {
        throw e;
      }
    }
  }
  reportMessage(event: CompilerEvent): void {
    console.log(kmc.CompilerMessages.severityName(event.code) + ' ' + event.code.toString(16) + ': ' + event.message);
  }
  loadLdmlKeyboardSchema(): Buffer {
    let schemaPath = new URL('ldml-keyboard.schema.json', import.meta.url);
    return fs.readFileSync(schemaPath);
  }
  loadLdmlKeyboardTestSchema(): Buffer {
    let schemaPath = new URL('ldml-keyboardtest.schema.json', import.meta.url);
    return fs.readFileSync(schemaPath);
  }
  loadKvksJsonSchema(): Buffer {
    let schemaPath = new URL('kvks.schema.json', import.meta.url);
    return fs.readFileSync(schemaPath);
  }
}

function compileKeyboard(inputFilename: string, options: kmc.CompilerOptions): [Uint8Array,Uint8Array] {
  const c : CompilerCallbacks = new NodeCompilerCallbacks();
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

function loadTestData(inputFilename: string, options: kmc.CompilerOptions): LDMLKeyboardTestDataXMLSourceFile {
   const c : CompilerCallbacks = new NodeCompilerCallbacks();
   const k = new kmc.Compiler(c, options);
   let source = k.loadTestData(inputFilename);
   if(!source) {
     return null;
   }
   return source;
}

let options: kmc.CompilerOptions = {
  debug: program.debug ?? false,
  addCompilerVersion: program.compilerVersion ?? true
}

if (program.testData) {
  let testData = loadTestData(inputFilename, options);
  if (testData) {
    const fileBaseName = program.testData ?? inputFilename;
    const outFileBase = path.basename(fileBaseName, path.extname(fileBaseName));
    const outFileDir = path.dirname(fileBaseName);

    const outFileJson = path.join(outFileDir, outFileBase + '.json');
    console.log(`Writing JSON test data to ${outFileJson}`);
    fs.writeFileSync(outFileJson, JSON.stringify(testData, null, '  '));
  } else {
    console.error(`An error occurred loading test data ${inputFilename}`);
    process.exit(1);
  }
} else {
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
}
