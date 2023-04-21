import * as fs from 'fs';
import { BuildActivity, BuildActivityOptions } from './BuildActivity.js';
import { compileModel } from '@keymanapp/kmc-model';
import { NodeCompilerCallbacks } from '../../util/NodeCompilerCallbacks.js';

export class BuildModel extends BuildActivity {
  public get name(): string { return 'Lexical model'; }
  public get sourceExtension(): string { return '.model.ts'; }
  public get compiledExtension(): string { return '.model.js'; }
  public get description(): string { return 'Build a lexical model'; }
  public async build(infile: string, options: BuildActivityOptions): Promise<boolean> {
    let outputFilename: string = this.getOutputFilename(infile, options);
    let code = null;

    const callbacks = new NodeCompilerCallbacks();

    // Compile:
    try {
      code = compileModel(infile, callbacks);
    } catch(e) {
      console.error(e);
      return false;
    }

    if(!code) {
      console.error('Compilation failed.')
      return false;
    }

    // Output:
    fs.writeFileSync(outputFilename, code, 'utf8');

    return true;
  }
}