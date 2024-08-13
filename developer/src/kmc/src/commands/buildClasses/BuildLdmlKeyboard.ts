import * as kmcLdml from '@keymanapp/kmc-ldml';
import { CompilerCallbacks, LDMLKeyboardXMLSourceFileReader, CompilerOptions, KeymanFileTypes } from '@keymanapp/common-types';
import { BuildActivity } from './BuildActivity.js';
import { fileURLToPath } from 'url';

export class BuildLdmlKeyboard extends BuildActivity {
  public get name(): string { return 'LDML keyboard'; }
  public get sourceExtension(): KeymanFileTypes.Source { return KeymanFileTypes.Source.LdmlKeyboard; }
  public get compiledExtension(): KeymanFileTypes.Binary { return KeymanFileTypes.Binary.Keyboard; }
  public get description(): string { return 'Build a LDML keyboard'; }
  public async build(infile: string, outfile: string, callbacks: CompilerCallbacks, options: CompilerOptions): Promise<boolean> {
    // TODO-LDML: consider hardware vs touch -- touch-only layout will not have a .kvk
    const ldmlCompilerOptions: kmcLdml.LdmlCompilerOptions = {...options, readerOptions: {
      importsPath: fileURLToPath(new URL(...LDMLKeyboardXMLSourceFileReader.defaultImportsURL))
    }};
    const compiler = new kmcLdml.LdmlKeyboardCompiler();
    return await super.runCompiler(compiler, infile, outfile, callbacks, ldmlCompilerOptions);
  }
}
