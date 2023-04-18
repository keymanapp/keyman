import * as fs from 'fs';
import { BuildActivity, BuildActivityOptions } from './BuildActivity.js';
import KmpCompiler from '@keymanapp/kmc-package';

export class BuildPackage extends BuildActivity {
  public get name(): string { return 'Package'; }
  public get sourceExtension(): string { return '.kps'; }
  public get compiledExtension(): string { return '.kmp'; }
  public get description(): string  { return 'Build a Keyman package'; }
  public async build(infile: string, options: BuildActivityOptions): Promise<boolean> {
    let outfile = this.getOutputFilename(infile, options);

    //
    // Load .kps source data
    //

    let kpsString: string = fs.readFileSync(infile, 'utf8');
    let kmpCompiler = new KmpCompiler();
    let kmpJsonData = kmpCompiler.transformKpsToKmpObject(kpsString, infile);

    //
    // Build the .kmp package file
    //

    let data = await kmpCompiler.buildKmpFile(infile, kmpJsonData);
    if(data) {
      fs.writeFileSync(outfile, data, 'binary');
    } else {
      // TODO error logging
      return false;
    }

    return true;
  }
}
