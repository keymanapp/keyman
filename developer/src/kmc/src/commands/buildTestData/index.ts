import * as fs from 'fs';
import * as path from 'path';
import * as kmcLdml from '@keymanapp/kmc-ldml';
import { CompilerCallbacks, defaultCompilerOptions, LDMLKeyboardTestDataXMLSourceFile, LDMLKeyboardXMLSourceFileReader } from '@keymanapp/common-types';
import { NodeCompilerCallbacks } from '../../util/NodeCompilerCallbacks.js';
import { fileURLToPath } from 'url';
import { CommandLineBaseOptions } from 'src/util/baseOptions.js';

export async function buildTestData(infile: string, _options: any, commander: any) {
  const options: CommandLineBaseOptions = commander.optsWithGlobals();

  let compilerOptions: kmcLdml.LdmlCompilerOptions = {
    ...defaultCompilerOptions,
    ...options,
    saveDebug: false,
    shouldAddCompilerVersion: false,
    readerOptions: {
      importsPath: fileURLToPath(new URL(...LDMLKeyboardXMLSourceFileReader.defaultImportsURL))
    }
  };

  let testData = await loadTestData(infile, compilerOptions);
  if (!testData) {
    return;
  }

  const fileBaseName = options.outFile ?? infile;
  const outFileBase = path.basename(fileBaseName, path.extname(fileBaseName));
  const outFileDir = path.dirname(fileBaseName);
  const outFileJson = path.join(outFileDir, outFileBase + '.json');
  console.log(`Writing JSON test data to ${outFileJson}`);
  fs.writeFileSync(outFileJson, JSON.stringify(testData, null, '  '));
}

async function loadTestData(inputFilename: string, options: kmcLdml.LdmlCompilerOptions): Promise<LDMLKeyboardTestDataXMLSourceFile> {
  const callbacks: CompilerCallbacks = new NodeCompilerCallbacks(options);
  const k = new kmcLdml.LdmlKeyboardCompiler();
  if(!await k.init(callbacks, options)) {
    return null;
  }
  return await k.loadTestData(inputFilename);
}
