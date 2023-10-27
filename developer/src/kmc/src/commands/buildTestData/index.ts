import * as fs from 'fs';
import * as path from 'path';
import * as kmcLdml from '@keymanapp/kmc-ldml';
import { CompilerBaseOptions, CompilerCallbacks, defaultCompilerOptions, LDMLKeyboardTestDataXMLSourceFile, LDMLKeyboardXMLSourceFileReader } from '@keymanapp/common-types';
import { NodeCompilerCallbacks } from '../../util/NodeCompilerCallbacks.js';
import { fileURLToPath } from 'url';

export function buildTestData(infile: string, _options: any, commander: any) {
  const options: CompilerBaseOptions = commander.optsWithGlobals();

  let compilerOptions: kmcLdml.LdmlCompilerOptions = {
    ...defaultCompilerOptions,
    ...options,
    saveDebug: false,
    shouldAddCompilerVersion: false,
    readerOptions: {
      importsPath: fileURLToPath(new URL(...LDMLKeyboardXMLSourceFileReader.defaultImportsURL))
    }
  };

  let testData = loadTestData(infile, compilerOptions);
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

function loadTestData(inputFilename: string, options: kmcLdml.LdmlCompilerOptions): LDMLKeyboardTestDataXMLSourceFile {
  const callbacks: CompilerCallbacks = new NodeCompilerCallbacks(options);
  const k = new kmcLdml.LdmlKeyboardCompiler(callbacks, options);
  let source = k.loadTestData(inputFilename);
  if (!source) {
    return null;
  }
  return source;
}
