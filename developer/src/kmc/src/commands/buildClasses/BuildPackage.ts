import * as fs from 'fs';
import { BuildActivity } from './BuildActivity.js';
import { CompilerCallbacks, CompilerOptions, KeymanFileTypes } from '@keymanapp/common-types';
import { KmpCompiler, PackageValidation } from '@keymanapp/kmc-package';

export class BuildPackage extends BuildActivity {
  public get name(): string { return 'Package'; }
  public get sourceExtension(): KeymanFileTypes.Source { return KeymanFileTypes.Source.Package; }
  public get compiledExtension(): KeymanFileTypes.Binary { return KeymanFileTypes.Binary.Package; }
  public get description(): string  { return 'Build a Keyman package'; }
  public async build(infile: string, callbacks: CompilerCallbacks, options: CompilerOptions): Promise<boolean> {

    const outfile = this.getOutputFilename(infile, options);

    //
    // Load .kps source data
    //

    const kmpCompiler = new KmpCompiler(callbacks);
    const kmpJsonData = kmpCompiler.transformKpsToKmpObject(infile);
    if(!kmpJsonData) {
      return false;
    }

    //
    // Validate the package file
    //

    const validation = new PackageValidation(callbacks, options);
    if(!validation.validate(infile, kmpJsonData)) {
      return false;
    }

    //
    // Build the .kmp package file
    //

    const data = await kmpCompiler.buildKmpFile(infile, kmpJsonData);
    if(!data) {
      return false;
    }

    fs.writeFileSync(outfile, data, 'binary');
    return true;
  }
}
