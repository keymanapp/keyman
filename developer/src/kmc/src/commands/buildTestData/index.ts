import * as fs from 'fs';
import * as path from 'path';
import * as kmcLdml from '@keymanapp/kmc-ldml';
import { CompilerCallbacks, defaultCompilerOptions, LDMLKeyboardTestDataXMLSourceFile, LDMLKeyboardXMLSourceFileReader } from '@keymanapp/developer-utils';
import { NodeCompilerCallbacks } from '../../util/NodeCompilerCallbacks.js';
import { fileURLToPath } from 'url';
import { CommandLineBaseOptions } from 'src/util/baseOptions.js';
import { exitProcess } from '../../util/sysexits.js';
import { InfrastructureMessages } from '../../messages/infrastructureMessages.js';

export async function buildTestData(infile: string, _options: any, commander: any): Promise<void> {
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

  const callbacks = new NodeCompilerCallbacks(options);

  let testData = await loadTestData(infile, callbacks, compilerOptions);
  if (!testData || callbacks.hasFailureMessage()) {
    await exitProcess(1);
  }

  const fileBaseName = options.outFile ?? infile;
  const outFileBase = path.basename(fileBaseName, path.extname(fileBaseName));
  const outFileDir = path.dirname(fileBaseName);
  const outFileJson = path.join(outFileDir, outFileBase + '.json');
  fs.writeFileSync(outFileJson, JSON.stringify(testData, null, '  '));
  callbacks.reportMessage(InfrastructureMessages.Info_FileBuiltSuccessfully({filename: outFileJson, relativeFilename:  infile}));
}

async function loadTestData(inputFilename: string, callbacks: CompilerCallbacks, options: kmcLdml.LdmlCompilerOptions): Promise<LDMLKeyboardTestDataXMLSourceFile> {
  const k = new kmcLdml.LdmlKeyboardCompiler();
  if(!await k.init(callbacks, options)) {
    return null;
  }
  return await k.loadTestData(inputFilename);
}
