import * as fs from 'fs';
import { BuildActivity } from './BuildActivity.js';
import { compileModel } from '@keymanapp/kmc-model';
import { CompilerCallbacks, CompilerOptions, KeymanFileTypes } from '@keymanapp/common-types';

export class BuildModel extends BuildActivity {
  public get name(): string { return 'Lexical model'; }
  public get sourceExtension(): KeymanFileTypes.Source { return KeymanFileTypes.Source.Model; }
  public get compiledExtension(): KeymanFileTypes.Binary { return KeymanFileTypes.Binary.Model; }
  public get description(): string { return 'Build a lexical model'; }
  public async build(infile: string, callbacks: CompilerCallbacks, options: CompilerOptions): Promise<boolean> {
    let outputFilename: string = this.getOutputFilename(infile, options);
    let code = null;

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