import * as fs from 'fs';
import { BuildActivity, BuildActivityOptions } from './BuildActivity.js';
import KmpCompiler from '@keymanapp/kmc-package';
import { CompilerCallbacks } from '@keymanapp/common-types';

export class BuildPackage extends BuildActivity {
  public get name(): string { return 'Package'; }
  public get sourceExtension(): string { return '.kps'; }
  public get compiledExtension(): string { return '.kmp'; }
  public get description(): string  { return 'Build a Keyman package'; }
  public async build(infile: string, callbacks: CompilerCallbacks, options: BuildActivityOptions): Promise<boolean> {

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
