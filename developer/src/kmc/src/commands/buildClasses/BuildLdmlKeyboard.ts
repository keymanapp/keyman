import * as fs from 'fs';
import * as kmcLdml from '@keymanapp/kmc-ldml';
import { CompilerCallbacks, LDMLKeyboardXMLSourceFileReader, CompilerOptions, KeymanFileTypes } from '@keymanapp/common-types';
import { BuildActivity } from './BuildActivity.js';
import { fileURLToPath } from 'url';
import { InfrastructureMessages } from '../../messages/infrastructureMessages.js';

export class BuildLdmlKeyboard extends BuildActivity {
  public get name(): string { return 'LDML keyboard'; }
  public get sourceExtension(): KeymanFileTypes.Source { return KeymanFileTypes.Source.LdmlKeyboard; }
  public get compiledExtension(): KeymanFileTypes.Binary { return KeymanFileTypes.Binary.Keyboard; }
  public get description(): string { return 'Build a LDML keyboard'; }
  public async build(infile: string, outfile: string, callbacks: CompilerCallbacks, options: CompilerOptions): Promise<boolean> {
    // TODO-LDML: consider hardware vs touch -- touch-only layout will not have a .kvk
    // Compile:
    // TODO: Consider if this mkdir should be in write()
    // TODO: This pattern may be needed for kmc-kmn as well?
    const outFileDir = callbacks.path.dirname(outfile ?? infile);

    try {
      fs.mkdirSync(outFileDir, {recursive: true});
    } catch(e) {
      callbacks.reportMessage(InfrastructureMessages.Error_CannotCreateFolder({folderName:outFileDir, e}));
      return false;
    }

    const ldmlCompilerOptions: kmcLdml.LdmlCompilerOptions = {...options, readerOptions: {
      importsPath: fileURLToPath(new URL(...LDMLKeyboardXMLSourceFileReader.defaultImportsURL))
    }};

    const compiler = new kmcLdml.LdmlKeyboardCompiler();
    if(!await compiler.init(callbacks, ldmlCompilerOptions)) {
      return false;
    }

    const result = await compiler.run(infile, outfile);
    if(!result) {
      return false;
    }

    return await compiler.write(result.artifacts);
  }
}
