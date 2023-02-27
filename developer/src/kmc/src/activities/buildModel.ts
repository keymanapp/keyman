import * as fs from 'fs';
import { BuildCommandOptions } from '../commands/build.js';
import { compileModel } from '@keymanapp/kmc-model';

export async function buildModel(infile: string, options: BuildCommandOptions): Promise<boolean> {

  let outputFilename: string = options.outFile ? options.outFile : infile.replace(/\.ts$/i, ".js");

  let code = null;

  // Compile:
  try {
    code = compileModel(infile);
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