import { BuildActivity } from './BuildActivity.js';
import { LexicalModelCompiler } from '@keymanapp/kmc-model';
import { KeymanFileTypes } from '@keymanapp/common-types';
import { CompilerOptions, CompilerCallbacks } from '@keymanapp/developer-utils';

export class BuildModel extends BuildActivity {
  public get name(): string { return 'Lexical model'; }
  public get sourceExtension(): KeymanFileTypes.Source { return KeymanFileTypes.Source.Model; }
  public get compiledExtension(): KeymanFileTypes.Binary { return KeymanFileTypes.Binary.Model; }
  public get description(): string { return 'Build a lexical model'; }
  public async build(infile: string, outfile: string, callbacks: CompilerCallbacks, options: CompilerOptions): Promise<boolean> {
    const compiler = new LexicalModelCompiler();
    return await super.runCompiler(compiler, infile, outfile, callbacks, options);
  }
}