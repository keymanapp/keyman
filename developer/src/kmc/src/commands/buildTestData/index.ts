import * as fs from 'fs';
import * as path from 'path';
import * as kmc from '@keymanapp/kmc-ldml';
import { CompilerCallbacks, LDMLKeyboardTestDataXMLSourceFile, LDMLKeyboardXMLSourceFileReader } from '@keymanapp/common-types';
import { NodeCompilerCallbacks } from '../../messages/NodeCompilerCallbacks.js';
import { fileURLToPath } from 'url';

export interface BuildTestDataOptions {
  outFile?: string;
};

export function buildTestData(infile: string, options: BuildTestDataOptions) {
  let compilerOptions: kmc.CompilerOptions = {
    debug: false,
    addCompilerVersion: false,
    readerOptions: {
      importsPath: fileURLToPath(LDMLKeyboardXMLSourceFileReader.defaultImportsURL)
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

function loadTestData(inputFilename: string, options: kmc.CompilerOptions): LDMLKeyboardTestDataXMLSourceFile {
  const c: CompilerCallbacks = new NodeCompilerCallbacks();
  const k = new kmc.LdmlKeyboardCompiler(c, options);
  let source = k.loadTestData(inputFilename);
  if (!source) {
    return null;
  }
  return source;
}
