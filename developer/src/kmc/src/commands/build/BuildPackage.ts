import * as fs from 'fs';
import { BuildActivity, BuildActivityOptions } from './BuildActivity.js';
import KmpCompiler from '@keymanapp/kmc-package';
import { CompilerCallbacks } from '@keymanapp/common-types';
import { NodeCompilerCallbacks } from 'src/util/NodeCompilerCallbacks.js';

export class BuildPackage extends BuildActivity {
  public get name(): string { return 'Package'; }
  public get sourceExtension(): string { return '.kps'; }
  public get compiledExtension(): string { return '.kmp'; }
  public get description(): string  { return 'Build a Keyman package'; }
  public async build(infile: string, options: BuildActivityOptions): Promise<boolean> {
    const c: CompilerCallbacks = new NodeCompilerCallbacks();

    const outfile = this.getOutputFilename(infile, options);

    //
    // Load .kps source data
    //

    const kpsString: string = fs.readFileSync(infile, 'utf8');
    const kmpCompiler = new KmpCompiler(c);
    const kmpJsonData = kmpCompiler.transformKpsToKmpObject(kpsString, infile);
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
