import { BuildActivity } from './BuildActivity.js';
import { KeymanFileTypes } from '@keymanapp/common-types';
import { CompilerOptions, CompilerCallbacks } from '@keymanapp/developer-utils';
import { KmpCompiler } from '@keymanapp/kmc-package';

export class BuildPackage extends BuildActivity {
  public get name(): string { return 'Package'; }
  public get sourceExtension(): KeymanFileTypes.Source { return KeymanFileTypes.Source.Package; }
  public get compiledExtension(): KeymanFileTypes.Binary { return KeymanFileTypes.Binary.Package; }
  public get description(): string  { return 'Build a Keyman package'; }
  public async build(infile: string, outfile: string, callbacks: CompilerCallbacks, options: CompilerOptions): Promise<boolean> {
    const compiler = new KmpCompiler();
    return await super.runCompiler(compiler, infile, outfile, callbacks, options);
  }
}
