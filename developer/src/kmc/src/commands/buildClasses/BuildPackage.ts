import { BuildActivity } from './BuildActivity.js';
import { CompilerCallbacks, CompilerOptions, KeymanFileTypes } from '@keymanapp/common-types';
import { KmpCompiler } from '@keymanapp/kmc-package';

export class BuildPackage extends BuildActivity {
  public get name(): string { return 'Package'; }
  public get sourceExtension(): KeymanFileTypes.Source { return KeymanFileTypes.Source.Package; }
  public get compiledExtension(): KeymanFileTypes.Binary { return KeymanFileTypes.Binary.Package; }
  public get description(): string  { return 'Build a Keyman package'; }
  public async build(infile: string, outfile: string, callbacks: CompilerCallbacks, options: CompilerOptions): Promise<boolean> {

    outfile = this.getOutputFilename(infile, outfile);

    const kmpCompiler = new KmpCompiler();
    if(!await kmpCompiler.init(callbacks, options)) {
      return false;
    }

    const result = await kmpCompiler.run(infile, outfile);
    if(!result) {
      return false;
    }

    return await kmpCompiler.write(result.artifacts);
  }
}
